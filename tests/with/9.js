var;
print((function(ignored) {
    var f,firstObject;
    firstObject = {
        "a": "1",
        "b": "2",
        "c": "3",
        "q": "4"
    };
    with (firstObject) {
        f = function(x) {
	    var;
            return q
        }
    };
    firstObject["q"] = "17";
    return f("395")
})("ignored"))