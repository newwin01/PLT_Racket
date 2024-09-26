package edu.handong.csee.plt.FAE;

public class Orop extends AST{ //assume op is in the if statement only

    AST firstCond;
    AST secondCond;

    public Orop (AST firstCond, AST secondCond) {
        this.firstCond = firstCond;
        this.secondCond = secondCond;
    }

    public AST getFirstCond() {
        return firstCond;
    }

    public AST getSecondCond() {
        return secondCond;
    }

    @Override
    public String getASTCode() {
        return "(or " + firstCond.getASTCode() + " " + secondCond.getASTCode() + ")";
    }
}
