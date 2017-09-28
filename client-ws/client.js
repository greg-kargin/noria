var classes = {};
var components = {};

function registerComponent(type, constructor) {
    classes[type] = constructor;
}

function preprocessProps(ws, node, props) {
    for (var k in props) {
        if (props[k] == "noria-handler") {
            props[k] = function() {
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
    }
    return props;
}

function runClient() {
    var ws = new WebSocket("ws://localhost:8000");
    ws.onmessage = function (event) {
        var updates = JSON.parse(event.data);
        for (i in updates) {
            var e = updates[i];
            console.log(e);
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
                    if (node == 1) {
                        document.getElementById("root").appendChild(component.domNode);
                    }
                }
            } else if (updateType == "update-props") {
                var node = e["update-props_node"];
                var diff = preprocessProps(ws, node, e["update-props_props-diff"]);
                var c = components[node];
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

registerComponent("div", function (props) {
    var callbacks = {};
    var e = document.createElement("div");
    
    var setProps = function (diffProps) {
        for (var key in props) {
            var p = props[key];
            if (key == "on-click") {
                if (p == "-noria-handler") {
                    e.removeEventListener("click", callbacks["on-click"]);
                    delete callbacks ["on-click"];
                } else {
                    var cb = function (evt) {
                        p();
                    };
                    e.addEventListener("click", cb);
                    callbacks["on-click"] = cb;
                }
            } else {
                if (p == null) {
                    e.removeAttribute(key);
                } else {
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
})

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

