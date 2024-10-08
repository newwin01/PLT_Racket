package edu.handong.csee.plt.FAE;

public class Add extends AST{
	private AST lhs = new AST();
	private AST rhs = new AST();
	
	public Add(AST lhs, AST rhs) {
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
		return "(add " + lhs.getASTCode() + " " + rhs.getASTCode() + ")";
	}
}

