package edu.handong.csee.plt.AriOperation;

import edu.handong.csee.plt.FAEValue.numV;

public class NumOP {

    IntegerMath op;
    interface IntegerMath {
        int operation(int a, int b);
    }

    public numV operateBinary(numV a, numV b) {
        int result = (op.operation(Integer.parseInt(a.getNumV()), Integer.parseInt(b.getNumV())));
        return new numV (Integer.toString(result));
    }

}
