package edu.handong.csee.plt.FAEValue;

public class numV extends FAEAST{

    private String numV = "0";

    public numV(String numV) {

        this.numV = numV;

    }

    public String getNumV() {return numV;}

    @Override
    public String getFAEAST() { return "(numV " + numV + ")";}

}
