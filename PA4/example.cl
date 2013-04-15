Class B {
  s:String <- "hello";
  g(y:String) : Int{
    y.concat(s).length()
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
 
class Main {
  obj : A;
  main():Int {{ 
    obj  <- new A;
    obj.f(1);
  }};
};

