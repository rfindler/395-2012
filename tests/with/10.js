var;
print((function(ignored) {
    var f, g, firstObject;
    firstObject = {
        "a": "1",
        "b": "2",
        "c": "3",
        "q": "4"
    };
    with (firstObject) {
        f = function(x) {
            var; q = x
        }
    };
    firstObject["q"] = 17;
    with (firstObject) {
        g = function(x) {
            var; return q
        }
    };
    f("395");
    return g("2")
})("ignored"))