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



using namespace std;
using namespace CVC3;


int exitStatus;




// Check whether e is valid -- HELPER FUNCTION (needed by at least test1)
bool printValidityQueryResult(ValidityChecker* vc, Expr e )
{
    cout << "Query: ";
    vc->printExpr(e);

    bool res = vc->query(e);

    switch (res)
    {
    case false:
        cout << "Invalid" << endl << endl;
        break;
    case true:
        cout << "Valid" << endl << endl;
        break;
    }
    return res;
}


// Make a new assertion -- HELPER FUNCTION (needed by at least test1)
void newAssertionWithPrintout(ValidityChecker* vc, Expr e)
{
    cout << "Assert: ";
    vc->printExpr(e);
    vc->assertFormula(e);
}


void test1()
{
    CLFlags flags = ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    flags.setFlag("dump-log", ".test1.cvc");
    ValidityChecker* vc = ValidityChecker::create(flags);
  
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

        Expr p = vc->varExpr("p", vc->boolType());
        Expr e = vc->orExpr(p, vc->notExpr(p));

        IF_DEBUG(b =) printValidityQueryResult(vc, e);
        DebugAssert(b, "Should be valid");

        // Check x = y -> f(x) = f(y)

        Expr x = vc->varExpr("x", vc->realType());
        Expr y = vc->varExpr("y", vc->realType());

        Type real2real = vc->funType(vc->realType(), vc->realType());
        Op f = vc->createOp("f", real2real);
        Expr fx = vc->funExpr(f, x);
        Expr fy = vc->funExpr(f, y);

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
    
        vector<Expr> assertions;
        cout << "Scope level: " << vc->scopeLevel() << endl;
        cout << "Counter-example:" << endl;
        vc->getCounterExample(assertions);
        for (unsigned i = 0; i < assertions.size(); ++i) {
            vc->printExpr(assertions[i]);
        }
        cout << "End of counter-example" << endl << endl;

        // Reset to initial scope
        cout << "Resetting" << endl;
        vc->pop();
        DebugAssert(scopeLevel == vc->scopeLevel(), "scope error");
        cout << "Scope level: " << vc->scopeLevel() << endl << endl;

        // Check w = x & x = y & y = z & f(x) = f(y) & x = 1 & z = 2
    
        Expr w = vc->varExpr("w", vc->realType());
        Expr z = vc->varExpr("z", vc->realType());

        cout << "Push Scope" << endl << endl;
        vc->push();

        newAssertionWithPrintout(vc, vc->eqExpr(w, x));
        newAssertionWithPrintout(vc, vc->eqExpr(x, y));
        newAssertionWithPrintout(vc, vc->eqExpr(y, z));
        newAssertionWithPrintout(vc, vc->eqExpr(fx, fy));
        newAssertionWithPrintout(vc, vc->eqExpr(x, vc->ratExpr(1)));

        cout << endl << "simplify(w) = ";
        vc->printExpr(vc->simplify(w));
        cout << endl;
        DebugAssert(vc->simplify(w)==vc->ratExpr(1), "Expected simplify(w) = 1");

        newAssertionWithPrintout(vc, vc->eqExpr(z, vc->ratExpr(2)));
        assertions.clear();
        cout << "Inconsistent?: " << vc->inconsistent(assertions) << endl;

        cout << "Assumptions Used:" << endl;
        for (unsigned i = 0; i < assertions.size(); ++i) {
            vc->printExpr(assertions[i]);
        }

        cout << endl << "Pop Scope" << endl << endl;
        vc->pop();
    
        cout << "simplify(w) = ";
        vc->printExpr(vc->simplify(w));
        DebugAssert(vc->simplify(w)==w, "Expected simplify(w) = w");
        cout << endl;
    
        assertions.clear();
        cout << "Inconsistent?: " << vc->inconsistent(assertions) << endl;
    } catch(const Exception& e) {
        exitStatus = 1;
        cout << "*** Exception caught in test1(): \n" << e << endl;
    }
    delete vc;
}


// HELPER-OF-A-HELPER (used by createTestFormula )
Expr ltLex(ValidityChecker* vc, Expr i1, Expr i2, Expr j1, Expr j2)
{
    Expr res = vc->ltExpr(i1, j1);
    return vc->orExpr(res, vc->andExpr(vc->eqExpr(i1, j1), vc->ltExpr(i2, j2)));
}

// HELPER FUNCTION (needed by at least test3 and test4)
Expr createTestFormula(ValidityChecker* vc, Expr i1, Expr i2, Expr r1, Expr r2)
{
    Expr lt1 = ltLex(vc, r1, r2, i1, i2);
    Expr lt2 = ltLex(vc, i2, i1, r2, r1);
    return vc->andExpr(lt1, lt2);
}


void test3()
{
    CLFlags flags = ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    ValidityChecker* vc = ValidityChecker::create(flags);

    try {
        Expr i = vc->varExpr("i", vc->realType());
        Expr j = vc->varExpr("j", vc->realType());
        Expr k = vc->varExpr("k", vc->realType());
    
        Expr one = vc->ratExpr(1);
    
        cout << "i: " << i.getIndex() << endl;
    
        Expr test = createTestFormula(vc, i, j,
                                      vc->minusExpr(i, one), vc->minusExpr(j, k));
    
        cout << "Trying test: ";
        vc->printExpr(test);
        cout << endl;
    
        vc->push();
        bool result = vc->query(test);
        if (result) {
            cout << "Test Valid" << endl;
            vc->pop();
        }
        else {
            Expr condition;
            vector<Expr> assertions;
            unsigned index;
      
            vc->getCounterExample(assertions);
      
            cout << "Test Invalid Under Conditions:" << endl;
            for (index = 0; index < assertions.size(); ++index) {
                vc->printExpr(assertions[index]);
            }
      
            // Try assertions one by one
            for (index = 0; index < assertions.size(); ++index) {
                condition = vc->notExpr(assertions[index]);
                cout << "Trying test under condition: ";
                vc->printExpr(condition);
                cout << endl;
                vc->pop();
                vc->push();
                result = vc->query(vc->impliesExpr(condition, test));
                if (result) {
                    cout << "Result Valid" << endl;
                    break;
                }
                else {
                    cout << "Result Invalid" << endl;
                }
            }
        }
    } catch(const Exception& e) {
        exitStatus = 1;
        cout << "*** Exception caught in test3(): \n" << e << endl;
    }
    delete vc;
}


void test4()
{
    CLFlags flags = ValidityChecker::createFlags();
    flags.setFlag("dagify-exprs",false);
    ValidityChecker* vc = ValidityChecker::create(flags);
  
    try {
        Expr i = vc->varExpr("i", vc->realType());
        Expr j = vc->varExpr("j", vc->realType());
        Expr k = vc->varExpr("k", vc->realType());

        Expr one = vc->ratExpr(1);

        cout << "i: " << i.getIndex() << endl;

        Expr test = createTestFormula(vc, i, j,
                                      vc->minusExpr(i, one), vc->minusExpr(j, k));

        cout << "Trying test: ";
        vc->printExpr(test);
        cout << endl;

        vc->push();
        bool result = vc->query(test);
        if (result) {
            cout << "Test Valid" << endl;
        }
        else {
            Expr condition;
            vector<Expr> assertions;
            unsigned index;

            vc->getCounterExample(assertions);

            cout << "Test Invalid Under Conditions:" << endl;
            for (index = 0; index < assertions.size(); ++index) {
                vc->printExpr(assertions[index]);
            }

            // Try assertions one by one
            for (index = 0; index < assertions.size(); ++index) {
                condition = vc->notExpr(assertions[index]);
                cout << "Trying test under condition: ";
                vc->printExpr(condition);
                cout << endl;
                vc->pop();
                vc->push();
                result = vc->query(vc->impliesExpr(condition, test));
                if (result) {
                    cout << "Result Valid" << endl;
                    break;
                }
                else {
                    cout << "Result Invalid" << endl;
                }
            }
        }
    } catch(const Exception& e) {
        exitStatus = 1;
        cout << "*** Exception caught in test4(): \n" << e << endl;
    }
    delete vc;
}



void test17()  {
    ValidityChecker *vc = ValidityChecker::create();
    try {
        try {
            vector<string> selectors;
            vector<Expr> types;

            selectors.push_back("car");
            types.push_back(vc->intType().getExpr());
            selectors.push_back("cdr");
            types.push_back(vc->stringExpr("list"));

            Type badList = vc->dataType("list", "cons", selectors, types);
            DebugAssert(false, "Typechecking exception expected");
        } catch(const TypecheckException&) {
            // fall through
        }
        delete vc;
        vc = ValidityChecker::create();
        {
            vector<string> constructors;
            vector<vector<string> > selectors(2);
            vector<vector<Expr> > types(2);

            constructors.push_back("cons");
            selectors[0].push_back("car");
            types[0].push_back(vc->intType().getExpr());
            selectors[0].push_back("cdr");
            types[0].push_back(vc->stringExpr("list"));
            constructors.push_back("null");

            Type list = vc->dataType("list", constructors, selectors, types);

            Expr x = vc->varExpr("x", vc->intType());
            Expr y = vc->varExpr("y", list);

            vector<Expr> args;
            args.push_back(x);
            args.push_back(y);
            Expr cons = vc->datatypeConsExpr("cons", args);

            Expr sel = vc->datatypeSelExpr("car", cons);
            IF_DEBUG(bool b =) printValidityQueryResult(vc, vc->eqExpr(sel, x));
            DebugAssert(b, "Should be valid");

        }
        delete vc;
        vc = ValidityChecker::create();
        try {
            vector<string> names;
            vector<vector<string> > constructors(2);
            vector<vector<vector<string> > > selectors(2);
            vector<vector<vector<Expr> > > types(2);
            vector<Type> returnTypes;

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
        } catch(const TypecheckException&) {
            // fall through
        }
        delete vc;
        vc = ValidityChecker::create();
        {
            vector<string> names;
            vector<vector<string> > constructors(2);
            vector<vector<vector<string> > > selectors(2);
            vector<vector<vector<Expr> > > types(2);
            vector<Type> returnTypes;

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

            Type list1 = returnTypes[0];
            Type list2 = returnTypes[1];

            Expr x = vc->varExpr("x", vc->intType());
            Expr y = vc->varExpr("y", list2);
            Expr z = vc->varExpr("z", list1);

            vector<Expr> args;
            args.push_back(x);
            args.push_back(y);
            Expr cons1 = vc->datatypeConsExpr("cons1", args);

            Expr isnull = vc->datatypeTestExpr("null", y);
            Expr hyp = vc->andExpr(vc->eqExpr(z, cons1), isnull);

            args.clear();
            Expr null = vc->datatypeConsExpr("null", args);

            args.push_back(x);
            args.push_back(null);
            Expr cons1_2 = vc->datatypeConsExpr("cons1", args);

            IF_DEBUG(bool b =) printValidityQueryResult(vc, vc->impliesExpr(hyp, vc->eqExpr(z, cons1_2)));
            DebugAssert(b, "Should be valid");

        }
        delete vc;
        vc = ValidityChecker::create();
        {
            vector<string> constructors;
            vector<vector<string> > selectors(2);
            vector<vector<Expr> > types(2);

            constructors.push_back("A");
            constructors.push_back("B");

            Type two = vc->dataType("two", constructors, selectors, types);

            Expr x = vc->varExpr("x", two);
            Expr y = vc->varExpr("y", two);
            Expr z = vc->varExpr("z", two);

            vector<Expr> args;
            args.push_back(!vc->eqExpr(x,y));
            args.push_back(!vc->eqExpr(y,z));
            args.push_back(!vc->eqExpr(x,z));

            IF_DEBUG(bool b =) printValidityQueryResult(vc, !vc->andExpr(args));
            DebugAssert(b, "Should be valid");

        }
    } catch(const Exception& e) {
        exitStatus = 1;
        cout << "*** Exception caught in test17(): \n" << e << endl;
    }
    delete vc;
}


void test18()
{
    CLFlags flags = ValidityChecker::createFlags();
    flags.setFlag("tcc", true);
    ValidityChecker *vc = ValidityChecker::create(flags);
    try {
        vector<string> names;
        vector<vector<string> > constructors(3);
        vector<vector<vector<string> > > selectors(3);
        vector<vector<vector<Expr> > > types(3);
        vector<Type> returnTypes;

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

        Type nat = returnTypes[0];
        Type listType = returnTypes[1];
        Type tree = returnTypes[2];

        Expr x = vc->varExpr("x", nat);

        vector<Expr> args;
        Expr zero = vc->datatypeConsExpr("zero", args);
        Expr null = vc->datatypeConsExpr("null", args);
        Expr leaf = vc->datatypeConsExpr("leaf", args);

        vc->push();
        try {
            printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(zero, null)));
            DebugAssert(false, "Should have caught tcc exception");
        } catch(const TypecheckException&) { }

        vc->pop();
        args.push_back(vc->datatypeSelExpr("pred",x));
        Expr spx = vc->datatypeConsExpr("succ", args);
        Expr spxeqx = vc->eqExpr(spx, x);
        vc->push();
        try {
            printValidityQueryResult(vc, spxeqx);
            DebugAssert(false, "Should have caught tcc exception");
        } catch(const TypecheckException&) { }

        vc->pop();
        bool b = printValidityQueryResult(vc, vc->impliesExpr(vc->datatypeTestExpr("succ", x), spxeqx));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->orExpr(vc->datatypeTestExpr("zero", x),
                                 vc->datatypeTestExpr("succ", x)));
        DebugAssert(b, "Should be valid");

        Expr y = vc->varExpr("y", nat);
        Expr xeqy = vc->eqExpr(x, y);
        args.clear();
        args.push_back(x);
        Expr sx = vc->datatypeConsExpr("succ", args);
        args.clear();
        args.push_back(y);
        Expr sy = vc->datatypeConsExpr("succ", args);
        Expr sxeqsy = vc->eqExpr(sx,sy);
        b = printValidityQueryResult(vc, vc->impliesExpr(xeqy, sxeqsy));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(sx, zero)));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->impliesExpr(sxeqsy, xeqy));
        DebugAssert(b, "Should be valid");

        b = printValidityQueryResult(vc, vc->notExpr(vc->eqExpr(sx, x)));
        DebugAssert(b, "Should be valid");

    } catch(const Exception& e) {
        exitStatus = 1;
        cout << "*** Exception caught in test18(): \n" << e << endl;
    }
    delete vc;
}



int main(int argc, char** argv)
{
    int regressLevel = 3;
    if (argc > 1) regressLevel = atoi(argv[1]);
    cout << "Running (kelly's modified) API test"; //", regress level = " << regressLevel << endl;
    exitStatus = 0;
  
    try
    {
        // 1, 3, 4, 17, 18
        cout << "\ntest1(): {" << endl;
        test1(); // <---------------- seems worth keeping
        cout << "\n} // end of test1 \n\n";

        cout << "\ntest3(): {" << endl;
        test3(); // <---------------- seems worth keeping
        cout << "\n} // end of test3 \n\n";

        cout << "\ntest4(): {" << endl;
        test4(); // <---------------- seems worth keeping
        cout << "\n} // end of test4 \n\n";

        cout << "\ntest17(): {" << endl;
        test17(); // <---------------- seems worth keeping
        cout << "\n} // end of test17 \n\n";

        cout << "\ntest18(): {" << endl;
        test18(); // <---------------- seems worth keeping
        cout << "\n} // end of test18 \n\n";
    }
    catch(const Exception& e)
    {
        cout << "*** Exception caught: \n" << e << endl;
        exitStatus = 1;
    }

  
    if(exitStatus == 0)
        cout << "Program exits successfully." << endl;
    else
        cout << "Program exits with error status = " << exitStatus << "." << endl;
    return exitStatus;
}
