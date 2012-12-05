var f;
f=function (x) {
    var newObject,newObject2;
    newObject = {
        "a": "1",
        "b": "2",
        "c": "3",
        "q": "4"
    };
    newObject2 = newObject;
    with (newObject) {
        q = "2"
    };
    return newObject2
};
print(f("395"))
