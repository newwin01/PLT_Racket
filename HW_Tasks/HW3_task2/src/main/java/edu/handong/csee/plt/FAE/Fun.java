package edu.handong.csee.plt.FAE;

public class Fun extends AST {

    private String param;
    private AST body = new AST();

    public Fun (String param, AST body){

        param = param.replace("{", ""); //TODO: Any other way??
        param = param.replace("}", "");

        this.param = param;
        this.body = body;
    }

    public String getParam() {return param;}
    public AST getBody() {return body;}

    @Override
    public String getASTCode() {return  "(fun " + param + " " + body.getASTCode() + ")" ;}

}
