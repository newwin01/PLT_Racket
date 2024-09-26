package edu.handong.csee.plt.FAE;

public class Mul extends AST{

    private AST lhs = new AST();
    private AST rhs = new AST();

    public Mul(AST lhs, AST rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
    }

    public AST getLhs() {
        return lhs;
    }

    public AST getRhs() {
        return rhs;
    }

    @Override
    public String getASTCode() {
        return "(mul " + lhs.getASTCode() + " " + rhs.getASTCode() + ")";
    }
}
