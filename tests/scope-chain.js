print((function (a) {
  var x;
  x = "1";
  return function (b) {
    return x
  }
})("a")("b"))
