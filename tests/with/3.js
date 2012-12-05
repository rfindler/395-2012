var f;
f = function(x) {
    var q, newObject;
    newObject = {
        "a": "1",
        "b": "2",
        "c": "3"
    };
    with (newObject) {
        q = "3"
    };
    return q
};
print(f(395))