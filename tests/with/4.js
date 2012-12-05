var f;
f = function (x) {
    var newObject, q;
    newObject = {
        "a": "1",
        "b": "2",
        "c": "3",
        "q": "4"
    };
    with (newObject) {
        q = "3"
    };
    return newObject
};
print(f("395"))
