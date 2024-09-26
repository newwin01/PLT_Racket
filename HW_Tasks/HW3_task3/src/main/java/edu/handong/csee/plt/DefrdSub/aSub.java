package edu.handong.csee.plt.DefrdSub;

import edu.handong.csee.plt.FAEValue.FAEAST;

public class aSub extends DeferdSubAST{


    private String name;
    private FAEAST value = new FAEAST();
    private DeferdSubAST ds = new DeferdSubAST();

    public aSub(String name, FAEAST value, DeferdSubAST ds) {
        this.name = name;
        this.value = value;
        this.ds = ds;
    }

    public String getName() {return name;}
    public FAEAST getValue() {return value;}
    public DeferdSubAST getDs() {return ds;}

    @Override
    public String getDeferdSubAST() {
        return "(aSub " + name + " " + value.getFAEAST() + " " + ds.getDeferdSubAST() + ")";
    }

}
