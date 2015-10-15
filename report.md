# COPサンプルコードのkorzによる実装
## korzで実装が可能なもの
```java

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
