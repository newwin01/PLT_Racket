package edu.handong.csee.plt.FAE;

public class App extends AST{

    private AST funExpr = new AST();
    private AST argExpr = new AST();

    public App(AST funExpr, AST argExpr) {
        this.funExpr = funExpr;
        this.argExpr = argExpr;
    }

    public AST getFunExpr() {return funExpr;}
    public AST getArgExpr() {return argExpr;}

    @Override
    public String getASTCode() {return "(app " + funExpr.getASTCode() + " " + argExpr.getASTCode() + ")";}

}
