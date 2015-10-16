# COPサンプルコードのkorzによる実装
## korzで実装が可能なもの
1. simple example
```java
import layer Beta;

public class Main {
  public void m() {
    B b = new B();
    with (Alpha) {
      b.y();
    }
  }
}

import layer Alpha;
import layer Beta;

public class A {
  public void x(int i) {
    ...
  }
  layer Alpha {
    public void x(int i) { ... }
  }
  layer Beta {
    after public void x(int i) { ... }
  }
}

public class B {
  private A a = new A();

  public void y() {
    with (Beta) {
      a.x();
    }
  }
  public void z() {
    without (Alpha) {
      a.x(0);
    }
  }
  layer Alpha {
    public void y() {
      ...
      proceed();
    }
  }
}
```
```common-lisp
(method () main ()
  (let ((b (copy (B))))
    (y :rcvr b :alpha true)))

(def () A-parent (newcoord))
(def () A (newcoord (A-parent)))

(method (:rcvr (A-parent)) x (i)
        ... )

(method (:rcvr (A-parent) :alpha true) x (i)
	... )

(method (:rcvr (A-parent) :beta true) x (i)
	(x i :beta false)
	... )

(def () B-parent (newcoord))
(def () B (newcoord (B-parent)))

(var (:rcvr (B)) a (copy (A)))

(method (:rcvr (B-parent)) y ()
  (x 0 :rcvr (a :rcvr rcvr) :beta true))

(method (:rcvr (B-parent)) z ()
  (x 0 :rcvr (a :rcvr rcvr) :alpha false))

(method (:rcvr (B-parent) :alpha true) y ()
   ... 
   (y :alpha false))

```

2. person example
```java
public class Person {
  private String name, address;

  public Person(String name, String addr) {
    this.name = name;
    this.address = addr;
  }

  public String getAddress() {
    return this.address;
  }

  public String toString() {
    return name;
  }

  layer Address {
    public String toString() {
      return proceed() + ", " + getAddress();
    }
  }
}

public class App {
  void m() {
    Person p = new Person("Bob");
    with(new Address()) {
      System.out.println(p.toString());
    }
  }
}
```
```common-lisp
(def () person-parent (newcoord))
(def () person (newcoord (person-parent)))

(var (:rcvr (person)) name)
(var (:rcvr (person)) address)

(method () person (name addr)
	(let ((p (copy (person))))
	  (set (name :rcvr p) name)
	  (set (address :rcvr p) addr)
	  p))
(method (:rcvr (person-parent)) get-address ()
	(address))
(method (:rcvr (person-parent)) to-string ()
	(name))
(method (:rcvr (person-parent) :address true) to-string ()
	(cat (address) :rcvr (to-string :address false)))

(method () main ()
	(let ((p (person "kuwa" "ooo")))
	  (print (to-string :rcvr p))))
```
3. 
## korzでは実装が困難なもの
# 
```java
public abstract contextclass UserActivity{
  protected final Layer rtf = new RTFWidgets();
  protected final Layer outline = new Outline();
  protected final Layer code = new CodeWIdgets();
  protected final Layer greyCode = new ActiveSyntaxHighlighting();
  protected final Layer highlightedCode = new InactiveSyntaxHighlighting();

  private CJEditWindow win;

  public UserActivity(CJEditWindow win){
    this.win = win;
  }
  protected CJEditWindow getWin(){
    return this.win;
  }
}

public contextclass Commenting extends UserActivity {
  when(getWin().getCurrentBlockType() == BlockType.Commenting) :
    with(rtf, greyCode), without(code, outline, highlightedCode);
  
  public Commenting(CJEditWindow win){
    super(win);
  }
}

public contextclass Programming extends UserActivity {
  when(getWin().getCurrentBlockType() == BlockType.Programming) :
    with(code, outline, highlightedCode), without(rtf, greyCode);
  
  public Programming(CJEditWindow win){
    super(win);
  }
}

```
