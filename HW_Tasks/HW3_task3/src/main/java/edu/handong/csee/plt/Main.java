package edu.handong.csee.plt;

import edu.handong.csee.plt.DefrdSub.DeferdSubAST;
import edu.handong.csee.plt.DefrdSub.mtSub;
import edu.handong.csee.plt.FAE.AST;
import edu.handong.csee.plt.FAEValue.FAEAST;
import edu.handong.csee.plt.FAEValue.numV;
import org.apache.commons.cli.*;

public class Main {

    private static boolean onlyParser = false; // for -p option
    private static boolean help = false;
    private static String input;

    public static void main(String[] args) {

        Main main = new Main();
        main.run(args);

    }

    private void run(String[] args) {

        Options options = createOptions();

        if(parseOptions(options, args)) {

            if (help) {
                printHelp(options);
                return;
            }

            if (input == null) {
                printHelp(options);
                return;
            }

            Parser parser = new Parser();
            AST ast = parser.parse(input);
            DeferdSubAST ds = new mtSub(); //TODO: Change it to take this part as input it is possible.

            if (ast == null) {
                System.out.println("Syntax Error!");
                return;
            }

            if (onlyParser) {

                System.out.println(ast.getASTCode());

            } else {

                Interpreter interpreter = new Interpreter();
                Object result =  interpreter.interp(ast, ds);
                if(result instanceof numV)
                    System.out.println(((numV) result).getFAEAST());
                else if (result instanceof FAEAST)
                    System.out.println((((FAEAST) result).getFAEAST()));
                else
                    System.out.println((String) result);
            }
        }
    }

    //options
    private Options createOptions() {
        Options options = new Options();

        options.addOption(Option.builder("p").longOpt("Only parser")
                .desc("Using only parser")
                .build());
        options.addOption(Option.builder("h").longOpt("help")
                .desc("Show a help page")
                .build());
        options.addOption(Option.builder("i").longOpt("input")
                .desc("Expression will be evaluated").required().hasArg()
                .build());

        return options;
    }

    private boolean parseOptions(Options options, String[] args) {
        CommandLineParser parser = new DefaultParser();

        try {

            CommandLine cmd = parser.parse(options, args);
            onlyParser = cmd.hasOption("p");
            input = cmd.getOptionValue("i");

        } catch (Exception e) {
            printHelp(options);
            return false;
        }
        return true;
    }


    private void printHelp(Options options) {
        String header = "FAE with Java Help page";
        String footer ="\n https://github.com/newwin01";
        HelpFormatter formatter = new HelpFormatter();
        formatter.printHelp("FAE with Java",header, options, footer, true);
    }

}