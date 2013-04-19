Class B {
  s:String <- "hello";
  g(y:String, a:Int, b:Int, c:Int) : Int{
    y.concat(s).length() + a + b + c
  };
  f(x:Int) : Int {
    x+1
  };
};

Class A inherits B {
  a : Int;
  b : B <- new B;
  f (x:Int) : Int {
    x + a
  };
};

Class C {
  objA : A;
  objB : B;
};
 
class Main {
  obj : B;
  main():Int {{ 
    obj  <- new A;
    obj.f(1);
    obj.g("world", 1, 2, 3);
  }};
};

