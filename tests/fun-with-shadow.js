var f; 
f = function (x) {
        var;
        with (x) {
            print(f)
        };
        return x["a"]
};
print(f({"a":"1","b":"2","f":"7"}))
