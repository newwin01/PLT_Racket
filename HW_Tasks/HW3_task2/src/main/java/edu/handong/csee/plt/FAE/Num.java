package edu.handong.csee.plt.FAE;

public class Num extends AST {
	private String num = "0";
	
	public Num(String num){
		this.num = num;
	}
	
	public String getStrNum() {
		return num;
	}
	
	@Override
	public String getASTCode() {
		return "(num " + num +")";
	}
}
