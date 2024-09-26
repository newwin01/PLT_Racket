package edu.handong.csee.plt.FAEValue;

public class FAEAST {

    public String getFAEAST() {
        String fAstCode = "";

        if(this instanceof numV)
            fAstCode = ((numV)this).getFAEAST();

        if(this instanceof ClosureV)
            fAstCode = ((ClosureV)this).getFAEAST();

        return fAstCode;
    }

}
