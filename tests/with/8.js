function f(x) {
    var newObject = {
        "a": 1,
        "b": 2,
        "c": 3,
        "q": 4
    };
    var newObject2 = newObject;
    with (newObject) {
        q = 2;
    }
    return newObject2;
}
f(395);