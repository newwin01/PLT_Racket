package edu.handong.csee.plt;

import edu.handong.csee.plt.DefrdSub.DeferdSubAST;
import edu.handong.csee.plt.DefrdSub.aSub;
import edu.handong.csee.plt.DefrdSub.mtSub;
import edu.handong.csee.plt.FAEValue.FAEAST;

public class lookUp {


    public static FAEAST operateLookUp(String name, DeferdSubAST ds) {

        if ( ds instanceof mtSub) {
            System.err.println("Free Identifier");
            System.exit(-1);
        }

        if (ds instanceof aSub) {

            if (((aSub) ds).getName().equals(name)) {
                return ((aSub) ds).getValue();
            } else {
                return operateLookUp(name, ((aSub) ds).getDs());
            }

        }

        return null; //Should not happen

    }


}
