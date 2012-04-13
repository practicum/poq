///////////////////////////////////////////////////////////////////////////////
//                                                                           //
// this is a MODIFIED COPY BASED ON:                                         //
// File: main.cpp                                                            //
// Original Author: Clark Barrett (official source on www.cs.nyu.edu/acsys/cvc3/doc/INSTALL.html )
// Created: Sat Apr 19 01:47:47 2003                                         //
//                                                                           //
// ** this copy is heavily modified (perhaps butchered) by an unofficial user
//                                                                           //
///////////////////////////////////////////////////////////////////////////////
 

#include "vc.h"
#include "theory_arith.h" // for arith kinds and expressions
#include "theory_array.h"


#include "exception.h"
#include "typecheck_exception.h"
#include "command_line_flags.h"
#include "debug.h"  // NOTE: it seems that i build the non-debug version, so the calls to 'DebugAssert' are compiled OUT.





int exitStatus;




// Check whether e is valid -- HELPER FUNCTION (needed by at least test1)
bool printValidityQueryResult(CVC3::ValidityChecker* vc, CVC3::Expr e )
{
    std::cout << "Query: ";
    vc->printExpr(e);

    bool res = vc->query(e);

    switch (res)
    {
    case false:
        std::cout << "Invalid" << std::endl << std::endl;
        break;
    case true:
        std::cout << "Valid" << std::endl << std::endl;
        break;
    }
    return res;
}


// Make a new assertion -- HELPER FUNCTION (needed by at least test1)
void newAssertionWithPrintout(CVC3::ValidityChecker* vc, CVC3::Expr e)
{
    std::cout << "Assert: ";
    vc->printExpr(e);
    vc->assertFormula(e);
}


void test1()
{
    CVC3::CLFlags flags = CVC3::ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    flags.setFlag("dump-log", ".test1.cvc");
    CVC3::ValidityChecker* vc = CVC3::ValidityChecker::create(flags);
  
    // It is important that all Expr objects are deleted before vc is
    // deleted.  Therefore, we enclose them in a scope of try{ }catch
    // block.
    //
    // Also, vc methods may throw an Exception, and we want to delete vc
    // even in those exceptional cases.
    try {

        IF_DEBUG(bool b =) printValidityQueryResult(vc, vc->trueExpr());
        DebugAssert(b, "Should be valid");

        vc->push();
        IF_DEBUG(b =) printValidityQueryResult(vc, vc->falseExpr());
        DebugAssert(!b, "Should be invalid");
        vc->pop();          

        // Check p OR ~p

        CVC3::Expr p = vc->varExpr("p", vc->boolType());
        CVC3::Expr e = vc->orExpr(p, vc->notExpr(p));

        IF_DEBUG(b =) printValidityQueryResult(vc, e);
        DebugAssert(b, "Should be valid");

        // Check x = y -> f(x) = f(y)

        CVC3::Expr x = vc->varExpr("x", vc->realType());
        CVC3::Expr y = vc->varExpr("y", vc->realType());

        CVC3::Type real2real = vc->funType(vc->realType(), vc->realType());
        CVC3::Op f = vc->createOp("f", real2real);
        CVC3::Expr fx = vc->funExpr(f, x);
        CVC3::Expr fy = vc->funExpr(f, y);

        e = vc->impliesExpr(vc->eqExpr(x,y),vc->eqExpr(fx, fy));
        IF_DEBUG(b =) printValidityQueryResult(vc, e);
        DebugAssert(b, "Should be valid");

        // Check f(x) = f(y) -> x = y

        e = vc->impliesExpr(vc->eqExpr(fx,fy),vc->eqExpr(x, y));
        IF_DEBUG(int scopeLevel = vc->scopeLevel();)
            vc->push();
        IF_DEBUG(b =) printValidityQueryResult(vc, e);
        DebugAssert(!b, "Should be invalid");

        // Get counter-example
    
        std::vector<CVC3::Expr> assertions;
        std::cout << "Scope level: " << vc->scopeLevel() << std::endl;
        std::cout << "Counter-example:" << std::endl;
        vc->getCounterExample(assertions);
        for (unsigned i = 0; i < assertions.size(); ++i) {
            vc->printExpr(assertions[i]);
        }
        std::cout << "End of counter-example" << std::endl << std::endl;

        // Reset to initial scope
        std::cout << "Resetting" << std::endl;
        vc->pop();
        DebugAssert(scopeLevel == vc->scopeLevel(), "scope error");
        std::cout << "Scope level: " << vc->scopeLevel() << std::endl << std::endl;

        // Check w = x & x = y & y = z & f(x) = f(y) & x = 1 & z = 2
    
        CVC3::Expr w = vc->varExpr("w", vc->realType());
        CVC3::Expr z = vc->varExpr("z", vc->realType());

        std::cout << "Push Scope" << std::endl << std::endl;
        vc->push();

        newAssertionWithPrintout(vc, vc->eqExpr(w, x));
        newAssertionWithPrintout(vc, vc->eqExpr(x, y));
        newAssertionWithPrintout(vc, vc->eqExpr(y, z));
        newAssertionWithPrintout(vc, vc->eqExpr(fx, fy));
        newAssertionWithPrintout(vc, vc->eqExpr(x, vc->ratExpr(1)));

        std::cout << std::endl << "simplify(w) = ";
        vc->printExpr(vc->simplify(w));
        std::cout << std::endl;
        DebugAssert(vc->simplify(w)==vc->ratExpr(1), "Expected simplify(w) = 1");

        newAssertionWithPrintout(vc, vc->eqExpr(z, vc->ratExpr(2)));
        assertions.clear();
        std::cout << "Inconsistent?: " << vc->inconsistent(assertions) << std::endl;

        std::cout << "Assumptions Used:" << std::endl;
        for (unsigned i = 0; i < assertions.size(); ++i) {
            vc->printExpr(assertions[i]);
        }

        std::cout << std::endl << "Pop Scope" << std::endl << std::endl;
        vc->pop();
    
        std::cout << "simplify(w) = ";
        vc->printExpr(vc->simplify(w));
        DebugAssert(vc->simplify(w)==w, "Expected simplify(w) = w");
        std::cout << std::endl;
    
        assertions.clear();
        std::cout << "Inconsistent?: " << vc->inconsistent(assertions) << std::endl;
    } catch(const CVC3::Exception& e) {
        exitStatus = 1;
        std::cout << "*** Exception caught in test1(): \n" << e << std::endl;
    }
    delete vc;
}


// HELPER-OF-A-HELPER (used by createTestFormula )
CVC3::Expr ltLex(CVC3::ValidityChecker* vc, CVC3::Expr i1, CVC3::Expr i2, CVC3::Expr j1, CVC3::Expr j2)
{
    CVC3::Expr res = vc->ltExpr(i1, j1);
    return vc->orExpr(res, vc->andExpr(vc->eqExpr(i1, j1), vc->ltExpr(i2, j2)));
}

// HELPER FUNCTION (needed by at least test3 and test4)
CVC3::Expr createTestFormula(CVC3::ValidityChecker* vc, CVC3::Expr i1, CVC3::Expr i2, CVC3::Expr r1, CVC3::Expr r2)
{
    CVC3::Expr lt1 = ltLex(vc, r1, r2, i1, i2);
    CVC3::Expr lt2 = ltLex(vc, i2, i1, r2, r1);
    return vc->andExpr(lt1, lt2);
}


void test3()
{
    CVC3::CLFlags flags = CVC3::ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    CVC3::ValidityChecker* vc = CVC3::ValidityChecker::create(flags);

    try {
        CVC3::Expr i = vc->varExpr("i", vc->realType());
        CVC3::Expr j = vc->varExpr("j", vc->realType());
        CVC3::Expr k = vc->varExpr("k", vc->realType());
    
        CVC3::Expr one = vc->ratExpr(1);
    
        std::cout << "i: " << i.getIndex() << std::endl;
    
        CVC3::Expr test = createTestFormula(vc, i, j,
                                      vc->minusExpr(i, one), vc->minusExpr(j, k));
    
        std::cout << "Trying test: ";
        vc->printExpr(test);
        std::cout << std::endl;
    
        vc->push();
        bool result = vc->query(test);
        if (result) {
            std::cout << "Test Valid" << std::endl;
            vc->pop();
        }
        else {
            CVC3::Expr condition;
            std::vector<CVC3::Expr> assertions;
            unsigned index;
      
            vc->getCounterExample(assertions);
      
            std::cout << "Test Invalid Under Conditions:" << std::endl;
            for (index = 0; index < assertions.size(); ++index) {
                vc->printExpr(assertions[index]);
            }
      
            // Try assertions one by one
            for (index = 0; index < assertions.size(); ++index) {
                condition = vc->notExpr(assertions[index]);
                std::cout << "Trying test under condition: ";
                vc->printExpr(condition);
                std::cout << std::endl;
                vc->pop();
                vc->push();
                result = vc->query(vc->impliesExpr(condition, test));
                if (result) {
                    std::cout << "Result Valid" << std::endl;
                    break;
                }
                else {
                    std::cout << "Result Invalid" << std::endl;
                }
            }
        }
    } catch(const CVC3::Exception& e) {
        exitStatus = 1;
        std::cout << "*** Exception caught in test3(): \n" << e << std::endl;
    }
    delete vc;
}


void test4()
{
    CVC3::CLFlags flags = CVC3::ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    CVC3::ValidityChecker* vc = CVC3::ValidityChecker::create(flags);
  
    try {
        CVC3::Expr i = vc->varExpr("i", vc->realType());
        CVC3::Expr j = vc->varExpr("j", vc->realType());
        CVC3::Expr k = vc->varExpr("k", vc->realType());

        CVC3::Expr one = vc->ratExpr(1);

        std::cout << "i: " << i.getIndex() << std::endl;

        CVC3::Expr test = createTestFormula(vc, i, j,
                                      vc->minusExpr(i, one), vc->minusExpr(j, k));

        std::cout << "Trying test: ";
        vc->printExpr(test);
        std::cout << std::endl;

        vc->push();
        bool result = vc->query(test);
        if (result) {
            std::cout << "Test Valid" << std::endl;
        }
        else {
            CVC3::Expr condition;
            std::vector<CVC3::Expr> assertions;
            unsigned index;

            vc->getCounterExample(assertions);

            std::cout << "Test Invalid Under Conditions:" << std::endl;
            for (index = 0; index < assertions.size(); ++index) {
                vc->printExpr(assertions[index]);
            }

            // Try assertions one by one
            for (index = 0; index < assertions.size(); ++index) {
                condition = vc->notExpr(assertions[index]);
                std::cout << "Trying test under condition: ";
                vc->printExpr(condition);
                std::cout << std::endl;
                vc->pop();
                vc->push();
                result = vc->query(vc->impliesExpr(condition, test));
                if (result) {
                    std::cout << "Result Valid" << std::endl;
                    break;
                }
                else {
                    std::cout << "Result Invalid" << std::endl;
                }
            }
        }
    } catch(const CVC3::Exception& e) {
        exitStatus = 1;
        std::cout << "*** Exception caught in test4(): \n" << e << std::endl;
    }
    delete vc;
}



void test17()  {
    CVC3::ValidityChecker *vc = CVC3::ValidityChecker::create();
    try {
        try {
            std::vector<std::string> selectors;
            std::vector<CVC3::Expr> types;

            selectors.push_back("car");
            types.push_back(vc->intType().getExpr());
            selectors.push_back("cdr");
            types.push_back(vc->stringExpr("list"));

            CVC3::Type badList = vc->dataType("list", "cons", selectors, types);
            DebugAssert(false, "Typechecking exception expected");
        } catch(const CVC3::TypecheckException&) {
            // fall through
        }
        delete vc;
        vc = CVC3::ValidityChecker::create();
        {
            std::vector<std::string> constructors;
            std::vector<std::vector<std::string> > selectors(2);
            std::vector<std::vector<CVC3::Expr> > types(2);

            constructors.push_back("cons");
            selectors[0].push_back("car");
            types[0].push_back(vc->intType().getExpr());
            selectors[0].push_back("cdr");
            types[0].push_back(vc->stringExpr("list"));
            constructors.push_back("null");

            CVC3::Type list = vc->dataType("list", constructors, selectors, types);

            CVC3::Expr x = vc->varExpr("x", vc->intType());
            CVC3::Expr y = vc->varExpr("y", list);

            std::vector<CVC3::Expr> args;
            args.push_back(x);
            args.push_back(y);
            CVC3::Expr cons = vc->datatypeConsExpr("cons", args);

            CVC3::Expr sel = vc->datatypeSelExpr("car", cons);
            IF_DEBUG(bool b =) printValidityQueryResult(vc, vc->eqExpr(sel, x));
            DebugAssert(b, "Should be valid");

        }
        delete vc;
        vc = CVC3::ValidityChecker::create();
        try {
            std::vector<std::string> names;
            std::vector<std::vector<std::string> > constructors(2);
            std::vector<std::vector<std::vector<std::string> > > selectors(2);
            std::vector<std::vector<std::vector<CVC3::Expr> > > types(2);
            std::vector<CVC3::Type> returnTypes;

            names.push_back("list1");

            selectors[0].resize(1);
            types[0].resize(1);
            constructors[0].push_back("cons1");
            selectors[0][0].push_back("car1");
            types[0][0].push_back(vc->intType().getExpr());
            selectors[0][0].push_back("cdr1");
            types[0][0].push_back(vc->stringExpr("list2"));

            names.push_back("list2");

            selectors[1].resize(1);
            types[1].resize(1);
            constructors[1].push_back("cons2");
            selectors[0][0].push_back("car2");
            types[0][0].push_back(vc->intType().getExpr());
            selectors[0][0].push_back("cdr2");
            types[0][0].push_back(vc->stringExpr("list1"));

            vc->dataType(names, constructors, selectors, types, returnTypes);
            DebugAssert(false, "Typechecking exception expected");
        } catch(const CVC3::TypecheckException&) {
            // fall through
        }
        delete vc;
        vc = CVC3::ValidityChecker::create();
        {
            std::vector<std::string> names;
            std::vector<std::vector<std::string> > constructors(2);
            std::vector<std::vector<std::vector<std::string> > > selectors(2);
            std::vector<std::vector<std::vector<CVC3::Expr> > > types(2);
            std::vector<CVC3::Type> returnTypes;

            names.push_back("list1");

            selectors[0].resize(1);
            types[0].resize(1);
            constructors[0].push_back("cons1");
            selectors[0][0].push_back("car1");
            types[0][0].push_back(vc->intType().getExpr());
            selectors[0][0].push_back("cdr1");
            types[0][0].push_back(vc->stringExpr("list2"));

            names.push_back("list2");

            selectors[1].resize(2);
            types[1].resize(2);
            constructors[1].push_back("cons2");
            selectors[1][0].push_back("car2");
            types[1][0].push_back(vc->intType().getExpr());
            selectors[1][0].push_back("cdr2");
            types[1][0].push_back(vc->stringExpr("list1"));
            constructors[1].push_back("null");

            vc->dataType(names, constructors, selectors, types, returnTypes);

            CVC3::Type list1 = returnTypes[0];
            CVC3::Type list2 = returnTypes[1];

            CVC3::Expr x = vc->varExpr("x", vc->intType());
            CVC3::Expr y = vc->varExpr("y", list2);
            CVC3::Expr z = vc->varExpr("z", list1);

            std::vector<CVC3::Expr> args;
            args.push_back(x);
            args.push_back(y);
            CVC3::Expr cons1 = vc->datatypeConsExpr("cons1", args);

            CVC3::Expr isnull = vc->datatypeTestExpr("null", y);
            CVC3::Expr hyp = vc->andExpr(vc->eqExpr(z, cons1), isnull);

            args.clear();
            CVC3::Expr null = vc->datatypeConsExpr("null", args);

            args.push_back(x);
            args.push_back(null);
            CVC3::Expr cons1_2 = vc->datatypeConsExpr("cons1", args);

            IF_DEBUG(bool b =) printValidityQueryResult(vc, vc->impliesExpr(hyp, vc->eqExpr(z, cons1_2)));
            DebugAssert(b, "Should be valid");

        }
        delete vc;
        vc = CVC3::ValidityChecker::create();
        {
            std::vector<std::string> constructors;
            std::vector<std::vector<std::string> > selectors(2);
            std::vector<std::vector<CVC3::Expr> > types(2);

            constructors.push_back("A");
            constructors.push_back("B");

            CVC3::Type two = vc->dataType("two", constructors, selectors, types);

            CVC3::Expr x = vc->varExpr("x", two);
            CVC3::Expr y = vc->varExpr("y", two);
            CVC3::Expr z = vc->varExpr("z", two);

            std::vector<CVC3::Expr> args;
            args.push_back(!vc->eqExpr(x,y));
            args.push_back(!vc->eqExpr(y,z));
            args.push_back(!vc->eqExpr(x,z));

            IF_DEBUG(bool b =) printValidityQueryResult(vc, !vc->andExpr(args));
            DebugAssert(b, "Should be valid");

        }
    } catch(const CVC3::Exception& e) {
        exitStatus = 1;
        std::cout << "*** Exception caught in test17(): \n" << e << std::endl;
    }
    delete vc;
}


void test18()
{
    CVC3::CLFlags flags = CVC3::ValidityChecker::createFlags();
    flags.setFlag("tcc", true);
    CVC3::ValidityChecker *vc = CVC3::ValidityChecker::create(flags);
    try {
        std::vector<std::string> names;
        std::vector<std::vector<std::string> > constructors(3);
        std::vector<std::vector<std::vector<std::string> > > selectors(3);
        std::vector<std::vector<std::vector<CVC3::Expr> > > types(3);
        std::vector<CVC3::Type> returnTypes;

        names.push_back("nat");

        selectors[0].resize(2);
        types[0].resize(2);
        constructors[0].push_back("zero");
        constructors[0].push_back("succ");
        selectors[0][1].push_back("pred");
        types[0][1].push_back(vc->stringExpr("nat"));

        names.push_back("list");

        selectors[1].resize(2);
        types[1].resize(2);
        constructors[1].push_back("cons");
        selectors[1][0].push_back("car");
        types[1][0].push_back(vc->stringExpr("tree"));
        selectors[1][0].push_back("cdr");
        types[1][0].push_back(vc->stringExpr("list"));
        constructors[1].push_back("null");

        names.push_back("tree");

        selectors[2].resize(2);
        types[2].resize(2);
        constructors[2].push_back("leaf");
        constructors[2].push_back("node");
        selectors[2][1].push_back("data");
        types[2][1].push_back(vc->stringExpr("nat"));
        selectors[2][1].push_back("children");
        types[2][1].push_back(vc->stringExpr("list"));

        vc->dataType(names, constructors, selectors, types, returnTypes);

        CVC3::Type nat = returnTypes[0];
        CVC3::Type listType = returnTypes[1];
        CVC3::Type tree = returnTypes[2];

        CVC3::Expr x = vc->varExpr("x", nat);

        std::vector<CVC3::Expr> args;
        CVC3::Expr zero = vc->datatypeConsExpr("zero", args);
        CVC3::Expr null = vc->datatypeConsExpr("null", args);
        CVC3::Expr leaf = vc->datatypeConsExpr("leaf", args);

        vc->push();
        try {
            printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(zero, null)));
            DebugAssert(false, "Should have caught tcc exception");
        } catch(const CVC3::TypecheckException&) { }

        vc->pop();
        args.push_back(vc->datatypeSelExpr("pred",x));
        CVC3::Expr spx = vc->datatypeConsExpr("succ", args);
        CVC3::Expr spxeqx = vc->eqExpr(spx, x);
        vc->push();
        try {
            printValidityQueryResult(vc, spxeqx);
            DebugAssert(false, "Should have caught tcc exception");
        } catch(const CVC3::TypecheckException&) { }

        vc->pop();
        bool b = printValidityQueryResult(vc, vc->impliesExpr(vc->datatypeTestExpr("succ", x), spxeqx));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->orExpr(vc->datatypeTestExpr("zero", x),
                                 vc->datatypeTestExpr("succ", x)));
        DebugAssert(b, "Should be valid");

        CVC3::Expr y = vc->varExpr("y", nat);
        CVC3::Expr xeqy = vc->eqExpr(x, y);
        args.clear();
        args.push_back(x);
        CVC3::Expr sx = vc->datatypeConsExpr("succ", args);
        args.clear();
        args.push_back(y);
        CVC3::Expr sy = vc->datatypeConsExpr("succ", args);
        CVC3::Expr sxeqsy = vc->eqExpr(sx,sy);
        b = printValidityQueryResult(vc, vc->impliesExpr(xeqy, sxeqsy));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(sx, zero)));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->impliesExpr(sxeqsy, xeqy));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(sx, x)));
        DebugAssert(b, "Should be valid");

    } catch(const CVC3::Exception& e) {
        exitStatus = 1;
        std::cout << "*** Exception caught in test18(): \n" << e << std::endl;
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
        // 1, 3, 4, 17, 18
        std::cout << "\ntest1(): {" << std::endl;
        test1(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test1 \n\n";

        std::cout << "\ntest3(): {" << std::endl;
        test3(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test3 \n\n";

        std::cout << "\ntest4(): {" << std::endl;
        test4(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test4 \n\n";

        std::cout << "\ntest17(): {" << std::endl;
        test17(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test17 \n\n";

        std::cout << "\ntest18(): {" << std::endl;
        test18(); // <---------------- seems worth keeping
        std::cout << "\n} // end of test18 \n\n";
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
