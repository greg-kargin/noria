var classes = {};
var components = {};

function registerComponent(type, constructor) {
    classes[type] = constructor;
}

function preprocessProps(ws, node, props) {
    var createCallback = function (k) {
        return function() {
            var args = [];
            var i = 0;
            for (i = 0; i < arguments.length; i++) {
                args.push(arguments[i]);
            }
            var message = {"node" : node,
                           "key" : k,
                           "arguments" : args};
            ws.send(JSON.stringify(message));
        }
    }
    for (var k in props) {
        if (props[k] == "noria-handler") {
            props[k] = createCallback(k);
        }
    }
    return props;
}

function runClient() {
    var ws = new WebSocket("ws://localhost:8000");
    ws.onmessage = function (event) {
        var updates = JSON.parse(event.data);
        for (var i = 0; i < updates.length; i++) {
            var e = updates[i];
            //console.log(e);
            var updateType = e["update_type"];
            if (updateType == "make-node") {
                var type = e["make-node_type"];
                var constr = classes[type];
                if (constr == null) {
                    console.error("no construnctor defined for type " + type);
                } else {
                    var node = e["make-node_node"];
                    var props = preprocessProps(ws, node, e["make-node_props"]);
                    var component = constr(props);
                    components[node] = component;
                    if (node == 0) {
                        document.getElementById("root").appendChild(component.domNode);
                    }
                }
            } else if (updateType == "update-props") {
                var node = e["update-props_node"];
                var diff = preprocessProps(ws, node, e["update-props_props-diff"]);
                var c = components[node];
//                console.log("update-props: " , diff ,  "node:",  node);
                if (c == null) {
                    console.error("component not found for update-props: " + e);
                } else {
                    c.updateProps(diff);
                }
            } else if (updateType == "remove") {
                var node = e["remove_node"];
                var c = components[node];
                if (c == null) {
                    console.error("component not found for remove-child: " + e);
                } else {
                    c.parent.removeChild(c);
                }
            } else if (updateType == "destroy") {
                var node = e["destroy_node"];
                var c = components[node];
                if (c == null) {
                    console.error("component not found for destroy: " + e);
                } else {
                    c.destroy();
                    delete components[node];
                }
            } else if (updateType == "add") {
                var index = e["add_index"];
                var childNode = e["add_child"];
                var parentNode = e["add_parent"];
                var parent = components[parentNode]
                var child = components[childNode]
                if (parent == null) {
                    console.error("parent not found for add: " + e);
                }
                if (child == null) {
                    console.error("child not found for add: " + e);
                }
                if (parent != null && child != null) {
                    parent.addChild(child, index);
                    child.parent = parent;
                }            
            }
        }
    }
}



function insertChildAtIndex(parent, child, index) {
    if (index >= parent.children.length) {
        parent.appendChild(child)
    } else {
        parent.insertBefore(child, parent.children[index])
    }
}

function tag(t) {
    return function (props) {
    var callbacks = {};
    var e = document.createElement(t);

    var table = {"on-click" : {event: "click",
                               handler: function (evt, cb) {
                                   cb();
                                   evt.stopPropagation();}},
                 "on-wheel" : {event: "wheel",
                               handler: function (evt, cb) {
                                   cb(evt.deltaX, evt.deltaY)
                                   evt.preventDefault();}}}
    var createCallback = function (handler, p) {
        return function (evt) {
            handler(evt, p);
        }
    }
    var setProps = function (diffProps) {
        for (var key in diffProps) {
            var p = diffProps[key];
            var entry = table[key];
            if (entry != null) {
                var event = entry.event;
                var handler = entry.handler;
                if (p == "-noria-handler") {
                    e.removeEventListener(event, callbacks[event]);
                    delete callbacks [event];
                } else {
                    var cb = createCallback(handler, p);
                    e.addEventListener(event, cb);
                    callbacks[event] = cb;
                }
            } else {
                if (p == null) {
                    e.removeAttribute(key);
                } else {
//                    console.log("setAttribute:", e, key, p);
                    e.setAttribute(key, p);
                }
            }
        }
    };
    
    setProps(props);
    
    return {
        domNode: e,
        updateProps: setProps,
        addChild: function (child, index) {
            insertChildAtIndex(e, child.domNode, index);
        },
        removeChild: function(child) {
            e.removeChild(child.domNode);
        },
        destroy: function() {}
    }
  }
}

registerComponent("div", tag("div"))
registerComponent("pre", tag("pre"))

registerComponent("text", function (props) {
    var e = document.createTextNode(props["text"]);
    return {
        domNode: e,
        updateProps: function (diffProps) {
            e.textContent = diffProps["text"];
        },
        addChild: function (child, index) {
            console.error("addChild to text node e: " + e + " child: " + child);
        },
        removeChild: function(child) {
            console.error("removeChild from text node e: " + e + " child: " + child);            
        },
        destroy: function() {}
    }
})

registerComponent("style", function(props) {
    var e = document.createElement("style");
    e.innerHTML = props.name + "{" + props.style + "}";
    return {
        domNode : e,
        updateProps : function (diffProps) {
            throw new Error("style updates are not supported");
        },
        addChild: function(child, index){
            throw new Error("style does not support children");
        },
        removeChild: function(child, index){
            throw new Error("style does not support children");
        },
        destroy: function (){}
    }
})

registerComponent("raw-line", function (props) {
    var callbacks = {};

    function create(props) {
        var metrics = props["metrics"];
        var width   = metrics["width"];
        var height  = metrics["height"];


        var text      = props["text"];
        var bgMarkup  = props["bg-markup"];
        var fgMarkup  = props["fg-markup"];

        // create bgDiv
        var bgDiv = document.createElement("div");
        var col   = 0;
        var i     = 0;
        while (i < bgMarkup.length) {
            var len    = bgMarkup[i];
            var sClass = bgMarkup[i + 1];
            if (sClass) {
                var div = document.createElement("div");
                div.classList.add(sClass);
                div.style.left   = col * width + "px";
                div.style.width  = width * len + "px";
                div.style.height = "100%";
                div.style.position = "absolute";
                bgDiv.appendChild(div);
            }
            i = i + 2;
            col = col + len;
        }

        // create fgDiv
        var fgDiv = document.createElement("pre");
        fgDiv.classList.add("line-text");
        var col   = 0;
        var i     = 0;

        while (i < fgMarkup.length) {
            var len    = fgMarkup[i];
            var sClass = fgMarkup[i + 1];
            var span = document.createElement("span");
            if (sClass) {
                span.setAttribute("class", sClass);
            }
            span.textContent = text.substring(col, col + len);
            fgDiv.appendChild(span);
            i = i + 2;
            col = col + len;
        }
        if (col < text.length) {
            var span = document.createElement("span");
            span.textContent = text.substring(col, text.length);
            fgDiv.appendChild(span);
        }

        // append bgDiv and fgDiv
        var rawLine = document.createElement("div");
        // todo append selection
        rawLine.appendChild(bgDiv);
        rawLine.appendChild(fgDiv);
        // todo append caret
        return rawLine;
    };

    var c = document.createElement("div");
    var e =  create(props);
    c.appendChild(e);    

    return {
        domNode: c,

        updateProps: function (props) {
            newE = create(props);           
            c.replaceChild(newE, e);
            e = newE;
        },

        addChild: function(child, index) {
            console.error("addChild to raw-line node e: " + e + " child: " + child);
        },

        removeChild: function(child) {
            console.error("removeChild to raw-line node e: " + e + " child: " + child);
        },

        destroy: function() {}
    }
})
