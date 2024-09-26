package edu.handong.csee.plt.FAE;

public class Condstate extends AST {

    private String cond;

    private AST firstComp;
    private AST secondComp;

    public Condstate(String cond, AST firstComp, AST secondComp) {
        this.cond = cond;
        this.firstComp = firstComp;
        this.secondComp = secondComp;
    }

    public String getCond() {
        return cond;
    }

    public AST getFirstComp() {
        return firstComp;
    }

    public AST getSecondComp() {
        return secondComp;
    }

    @Override
    public String getASTCode() {
        return "(" + cond + " " + firstComp.getASTCode() + " " + secondComp.getASTCode() + ")";
    }
}
