//todo
//1. make list of basic tests
//2. i + [i] -> (o) + [o] using basic tests
var tests = 
[["a.b;", "a[\"b\"]"],
 ["a[b];", "a[\"b\"]"],
 ["{a:b}", "{a:b}"],
 ["a[b] = 3;", "a[\"b\"] = 3"],
 ["var a = 2;", "var a = 2"],
 ["({a:b})", "({\"a\":b})"],
 ["({ \"x\" : \"z\"}[\"x\"] = \"5\")", "{\"x\":\"z\"}[\"x\"] = \"5\""],
 ["({ \"x\" : \"z\", \"1\" : \"Volvo\"}[\"x\"] = \"5\")", "{\"x\":\"z\", \"1\":\"Volvo\"}[\"x\"] = \"5\""],
 ["var x = { y : 1, y : 2 }; console.log(x)", "var x = {\"y\":1, \"y\":2}; console[\"log\"](x)" ],
 ["var x;", "var x"],
 ["({});", "({})"]]//,
//["({ get x () { \"395\" } }[x])", "({ get \"x\" () { \"395\" } }[\"x\"])"]]

load('../../../parse.js')

var run_tests = function()
{
	var count
	var result
	for (var i = 0; i < tests.length; i++)
		{
			result = parsejs(tests[i][0])
			count = i + 1
			if (result !== tests[i][1])
				{
					print("expected " + tests[i][1] + " but saw " + result)
				}
				
		}
	print(count + " tests run")
}
