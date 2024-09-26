package edu.handong.csee.plt.FAEValue;

import edu.handong.csee.plt.FAE.*;
import edu.handong.csee.plt.DefrdSub.*;
public class ClosureV extends FAEAST{

    private String param;
    private AST body = new AST();
    private DeferdSubAST ds = new DeferdSubAST();

    public ClosureV(String param, AST body, DeferdSubAST ds) {

        this.param = param;
        this.body = body;
        this.ds = ds;

    }

    public String getParam() {return param;}

    public AST getBody() {return body;}

    public DeferdSubAST getDs() {return ds;}

    @Override
    public String getFAEAST() {
        return "(ClosureV " + param + " " + body.getASTCode() + " " + ds.getDeferdSubAST() + ")" ;
    }

}
