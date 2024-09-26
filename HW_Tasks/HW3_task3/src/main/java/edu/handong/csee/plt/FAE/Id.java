package edu.handong.csee.plt.FAE;

public class Id extends AST{

    private String symbol;

    public Id(String symbol){
        this.symbol = symbol;
    }

    public String getSymbol() {return symbol;}

    @Override
    public String getASTCode() { return "(id " + symbol + ")"; }

}
