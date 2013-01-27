
#include "vc.h"
#include "theory_arith.h" // for arith kinds and expressions
#include "theory_array.h"

#include "exception.h"
#include "typecheck_exception.h"
#include "command_line_flags.h"
#include "debug.h"  // NOTE: it seems that i build the non-debug version, so the calls to 'DebugAssert' are compiled OUT.


int exitStatus;


// based on the code found in 'src/vcl/vc_cmd.cpp'
void VCCmd_reportResult(CVC3::ValidityChecker* d_vc, CVC3::QueryResult qres, bool checkingValidity)
{
    switch (qres) {
    case CVC3::VALID:
        std::cout << (checkingValidity ? "Valid." : "Unsatisfiable.") << std::endl;
        break;
    case CVC3::INVALID:
        std::cout << (checkingValidity ? "Invalid." : "Satisfiable.") << std::endl;
        break;
    case CVC3::ABORT:
        std::cout << "Unknown: resource limit exhausted." << std::endl;
        break;
    case CVC3::UNKNOWN: {
        // Vector of reasons in case of incomplete result
        std::vector<std::string> reasons;
        IF_DEBUG(bool b =) d_vc->incomplete(reasons);
        DebugAssert(b, "Expected incompleteness");
        std::cout << "Unknown.\n\n";
        std::cout << "CVC3 was incomplete in this example due to:";
        for(std::vector<std::string>::iterator i=reasons.begin(), iend=reasons.end();
            i!=iend; ++i)
            std::cout << "\n * " << (*i);
        std::cout << std::endl << std::endl;
        break;
    }
    default:
        FatalAssert(false, "Unexpected case");
    }

    std::cout << std::flush;

    //d_vc->printStatistics();
}


void test1()
{
    /*
cvc3 +intera
CVC> a, b, c, d, e: INT;
CVC> ASSERT c = a OR c = b;
CVC> ASSERT d = a OR d = b;
CVC> ASSERT e = a OR e = b;
CVC> PUSH;
CVC> ASSERT NOT(c=d);
CVC> CHECKSAT;
Satisfiable.
CVC> POP;
CVC> PUSH;
CVC> ASSERT NOT(c=d);
CVC> ASSERT NOT(d=e);
CVC> ASSERT NOT(e=c);
CVC> CHECKSAT;
Unsatisfiable.

     */
    CVC3::CLFlags flags = CVC3::ValidityChecker::createFlags();

    flags.setFlag("dump-log", "t2.cpp.log");


    CVC3::ValidityChecker* vc = CVC3::ValidityChecker::create(flags);

    // It is important that all Expr objects are deleted before vc is
    // deleted.  Therefore, we enclose them in a scope of try{ }catch
    // block.
    //
    // Also, vc methods may throw an Exception, and we want to delete vc
    // even in those exceptional cases.
    try
    {

        std::vector<CVC3::Expr> vars;

        for(char one_char = 'a'; one_char<='e'; one_char++)
        {
            std::string var_name(1,one_char);
            CVC3::Expr var_exp = vc->varExpr(var_name, vc->intType());
            vars.push_back(var_exp);
        }

        /*
CVC> ASSERT c = a OR c = b;
CVC> ASSERT d = a OR d = b;
CVC> ASSERT e = a OR e = b;
        */

        for(char target_char = 'c'; target_char<='e'; target_char++)
        {
            CVC3::Expr char_is_a = vc->eqExpr(vars[target_char-'a'],vars['a'-'a']);
            CVC3::Expr char_is_b = vc->eqExpr(vars[target_char-'a'],vars['b'-'a']);
            CVC3::Expr assert_exp = vc->orExpr(char_is_a, char_is_b);
            vc->assertFormula(assert_exp);
        }


        vc->push();

        /*
CVC> ASSERT NOT(c=d);
CVC> CHECKSAT;
Satisfiable.
CVC> POP;
CVC> PUSH;
         */
        CVC3::Expr c_eq_d = vc->eqExpr(vars['c'-'a'],vars['d'-'a']);
        CVC3::Expr not_c_eq_d = vc->notExpr(c_eq_d);

        vc->assertFormula(not_c_eq_d);

        CVC3::Expr ne = vc->trueExpr();
        CVC3::QueryResult qres = vc->checkUnsat(ne);
        VCCmd_reportResult(vc, qres , false /*bool checkingValidity. FALSE because we want SAT not validity*/);

        vc->pop();
        vc->push();

        /*
CVC> ASSERT NOT(c=d);
CVC> ASSERT NOT(d=e);
CVC> ASSERT NOT(e=c);
CVC> CHECKSAT;
Unsatisfiable.
        */

        CVC3::Expr c_is_d = vc->eqExpr(vars['c'-'a'],vars['d'-'a']);
        CVC3::Expr d_is_e = vc->eqExpr(vars['d'-'a'],vars['e'-'a']);
        CVC3::Expr e_is_c = vc->eqExpr(vars['e'-'a'],vars['c'-'a']);

        vc->assertFormula( vc->notExpr(c_is_d) );
        vc->assertFormula( vc->notExpr(d_is_e) );
        vc->assertFormula( vc->notExpr(e_is_c) );

        CVC3::Expr ne2 = vc->trueExpr();
        CVC3::QueryResult qres2 = vc->checkUnsat(ne2);
        VCCmd_reportResult(vc, qres2 , false /*bool checkingValidity. FALSE because we want SAT not validity*/);

    }
    catch(const CVC3::Exception& e)
    {
        exitStatus = 1;
        std::cout << "*** Exception caught in test1(): \n" << e << std::endl;
    }
    delete vc;
}



int main(int argc, char** argv)
{
    int regressLevel = 3;
    if (argc > 1) regressLevel = atoi(argv[1]);
    std::cout << "Running test"; //", regress level = " << regressLevel << std::endl;
    exitStatus = 0;

    try
    {
        std::cout << "\ntest1(): {" << std::endl;
        test1(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test1 \n\n";
    }
    catch(const CVC3::Exception& e)
    {
        std::cout << "*** Exception caught: \n" << e << std::endl;
        exitStatus = 1;
    }


    if(exitStatus == 0)
        std::cout << "Program exits successfully." << std::endl;
    else
        std::cout << "Program exits with error status = " << exitStatus << "." << std::endl;
    return exitStatus;
}
