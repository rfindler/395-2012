var f; 
f = function (x) {
        var;
        with (x) {
            a = "3"
        };
        return x["a"]
};
print(f({"a":"1","b":"2"}))
