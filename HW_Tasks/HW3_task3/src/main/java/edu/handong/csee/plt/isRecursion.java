package edu.handong.csee.plt;

import edu.handong.csee.plt.FAE.AST;
import edu.handong.csee.plt.FAE.App;
import edu.handong.csee.plt.FAE.Fun;

import java.util.ArrayList;
import java.util.regex.*;

public class isRecursion {


    public static boolean isRec(AST checkAst) { //Determine the code will return free identifier or not using parser

        App app = (App)checkAst;

        AST funExpr = app.getFunExpr();
        AST argExpr = app.getArgExpr(); //Right Hand Side

        if (funExpr instanceof Fun) {
            Fun fun = (Fun)funExpr;

            String identifier = fun.getParam(); //Get ID

            ArrayList<String> extractedID = extractAppID(argExpr.getASTCode());

            if (extractedID.contains(identifier))
                return !app.getArgExpr().getASTCode().contains("fun " + identifier + " ");
             else
                return false;
        }

        return false;
    }

    public static ArrayList<String> extractAppID(String checkRecString){

        ArrayList<String> appIDs = new ArrayList<>();


        String patternString = "\\(app\\s+\\(id\\s+(\\w+)\\)";
        Pattern pattern = Pattern.compile(patternString);
        Matcher matcher = pattern.matcher(checkRecString);


        while (matcher.find()) {
//            System.out.println(checkRecString);
            String idValue = matcher.group(1);
//            System.out.println("ID value after 'App': " + idValue);
            appIDs.add(idValue);
        }

        return appIDs;
    }


}
