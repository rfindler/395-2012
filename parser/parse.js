//functions like these "lift", each gets exprs, gets place where var declarations go instead of dump (accumulator)

//todo
//1. make parser run in git

var parsejs = function (js) {
	var exprs = Reflect.parse(js);
	var buf = [];
	var dump = function (s) {
		buf.push(s);
	}
	print_statements(dump, exprs);
	return buf.join("");
}

var print_statements = function (dump, exprs) {
	for (var i = 0; i < exprs.body.length; i++)
	{
		//var expr = exprs.body[i];
		//print(exprs.body[i].type)
		print_statement(dump, exprs.body[i]);
		if (i != (exprs.body.length - 1))
			dump("; ");
	}
}

var print_statement = function (dump, expr) {
	switch (expr.type)
	{
		case "BlockStatement":
			dump("{");
			print_statements(dump, expr);
			dump("}");
			break;
		case "LabeledStatement":
			dump(expr.label.name);
			dump(":");
			print_statement(dump, expr.body);
			break;
		case "ExpressionStatement":
			print_expression_all_args_explicit(dump, expr.expression, true, false);
			break;
		case "VariableDeclaration":
			dump("var ");
			print_variabledeclarators(dump, expr.declarations[0]);
			break;	
		case "Property":
			print("here")
			dump(expr.key.name);
			dump(":");
			dump(expr.value.name);
			break;
	}
}

var print_variabledeclarators = function(dump, vds)
{
	//add for loop
	print_pattern(dump, vds.id);
	if (vds.init)
	{
		dump(" = ");
		print_expression(dump, vds.init);
	}
}


var print_expression = function (dump, expr)
{
	print_expression_all_args_explicit(dump, expr, false, false)
}

var print_expression_already_delimited = function (dump, expr)
{
	print_expression_all_args_explicit(dump, expr, false, true)
}





//statementexp is true if this expression is a statement
//alreadydelimited is true if we already printed ","'s or something else around expression
var print_expression_all_args_explicit = function (dump, expr, statementexp, alreadydelimited)
{
	switch (expr.type)
		{
			case "FunctionExpression":
				dump("(");
				dump(")");
				print_statement(expr.body)			
				break;
			case "ObjectExpression":
				if (statementexp) { dump("(");}
					
				dump("{");
				for (var j = 0; j < expr.properties.length; j++)
				{
					print_quotedid(dump, expr.properties[j].key);
					dump(":");
					print_expression_already_delimited(dump, expr.properties[j].value);

					if (j != (expr.properties.length - 1))
						dump(", ");
				}
				dump("}");
				if (statementexp) { dump(")");}
				break;
			case "MemberExpression":
				print_expression(dump, expr.object)
				dump("[")
				if (expr.property.type == "Identifier")
					print_quotedid(dump, expr.property);
				else
					print_expression_already_delimited(dump, expr.property);
				dump("]")
				break;
			case "Identifier":
				dump(expr.name)
				break;
			case "Literal":
				print_literal(dump, expr)
				break;
			case "AssignmentExpression":
				print_expression(dump, expr.left)
				dump(" ")
				dump(expr.operator)
				dump(" ")
				print_expression(dump, expr.right)
				break;
			case "CallExpression":
				print_expression(dump, expr.callee)
				dump("(")
				for (var j = 0; j < expr.arguments.length; j++)
				{
					print_expression_already_delimited(dump, expr.arguments[j])
					if (j != (expr.arguments.length - 1))
						dump(", ");
				}
				dump(")")
		}
}

var print_literal = function(dump, literal)
{
	if (typeof(literal.value) == 'string')
		{
			dump("\"")
			dump(literal.value)
			dump("\"")
		}
	else
		{
			dump(literal.value)
		}
}

var print_quotedid = function(dump, property)
{
	dump("\"")
	switch (property.type)
		{
			case "Literal":
			dump(property.value)
			break;
			case "Identifier":
			dump(property.name)
			break;
		}
	dump("\"")	
}

var print_pattern = function (dump, expr)
{
	switch (expr.type)
		{
			case "Literal":
				dump(expr.value)
				break;
			case "Identifier":
				dump(expr.name)
				break;
		}
}


