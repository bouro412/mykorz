# COPサンプルコードのkorzによる実装
## korzで実装が可能なもの
# 
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
