#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;

   //object tag ids for builtins
   int objectclasstag;
   int ioclasstag;
   int intclasstag;
   int boolclasstag;
   int stringclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_class_nameTab();
   void code_class_objTab();
   void code_class_dispTab();
   void code_basic_protObj_attrs(Symbol clsName);
   void code_protObjs();

   void code_class_initializers();
   void code_class_method_definitions();

   void code_basic_class_initializers();
// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


typedef std::pair<Symbol, Symbol> FeatureInfo; // <name, decltype/rettype>
typedef std::vector<std::pair<CgenNodeP, FeatureInfo> > FeatureNameList;
typedef std::vector<CgenNodeP>   AncestorList;

//attribute offset
typedef std::map<Symbol, size_t> AttributeMap;

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   Symbol get_name() { return name;}

   //return method or attribute list
   FeatureNameList get_feature_list(bool check_on_method = true);
   AncestorList get_ancestors(){ 
       if (!m_ancestorsBuilt) build_ancestors();
       return m_ancestors;
   }

   size_t get_attr_offset(Symbol name){
       if (!m_attrMapBuilt){
           get_feature_list(false); //check attributes to identify offset
       }
       if (m_attrMap.find(name) == m_attrMap.end()){
           assert(!"Should never reach here!");
           return 0;
       }else{
           return m_attrMap[name];
       }
   }

private:
   void build_ancestors();

   AncestorList m_ancestors;
   AttributeMap m_attrMap;
   bool         m_ancestorsBuilt;
   bool         m_attrMapBuilt;
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

