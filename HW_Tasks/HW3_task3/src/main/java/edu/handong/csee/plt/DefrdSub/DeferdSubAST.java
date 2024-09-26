package edu.handong.csee.plt.DefrdSub;

public class DeferdSubAST {

    public String getDeferdSubAST() {
        String deferdSubAST = "";

        if(this instanceof mtSub)
            deferdSubAST = ((mtSub)this).getDeferdSubAST();
        if(this instanceof aSub)
            deferdSubAST = ((aSub)this).getDeferdSubAST();

        return deferdSubAST;
    }



}
