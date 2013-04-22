
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

#include <sstream>
#include <vector>
#include <algorithm>
#include <tr1/functional>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


namespace{
    typedef enum{LOC_FP, LOC_ARG, LOC_TEMP} REG_LOC;
    typedef struct{
        unsigned short location;  // 0 - fp (fp + offset), 1 - a1, a2, a3, 2 - t0, t1, ..t9
        unsigned short offset; 
    }RegAddrInfo;
    typedef std::map<Symbol, RegAddrInfo> VarAddrTable;

    //variable table to svae locations for arguments and temporaries
    VarAddrTable g_varTable;
    size_t g_current_sp_offset = 0; //sp - fp
    CgenClassTable* g_clsTablePtr;

    size_t first_attr_offset = 3;
    size_t next_lable_id = 0;

}

//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

namespace{
    void gen_dispTable_info(ostream&s, char* name){
        Symbol tagname = idtable.lookup_string(name);
        s << tagname << DISPTAB_SUFFIX;
    }
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
    // spim wants comments to start with '#'
    //os << "# start of generated code\n";

    initialize_constants();
    g_clsTablePtr = new CgenClassTable(classes,os);

    //os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

static void emit_pop(char* reg, ostream& str){
    emit_load(reg, 0, SP, str);
    emit_addiu(SP, SP, 4, str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
    IntEntryP lensym = inttable.add_int(len);

    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s  << LABEL                                             // label
        << WORD << stringclasstag << endl                                 // tag
        << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
        << WORD;

    //disp_table
    gen_dispTable_info(s, STRINGNAME);

    s << endl;                                              // dispatch table
    s << WORD;  lensym->code_ref(s);  s << endl;            // string length
    emit_string_constant(s,str);                                // ascii string
    s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                // label
        << WORD << intclasstag << endl                      // class tag
        << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
        << WORD; 

    gen_dispTable_info(s, INTNAME);

    s << endl;                                          // dispatch table
    s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
    // Add -1 eye catcher
    s << WORD << "-1" << endl;

    code_ref(s);  s << LABEL                                  // label
        << WORD << boolclasstag << endl                       // class tag
        << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
        << WORD;

    gen_dispTable_info(s, BOOLNAME);

    s << endl;                                            // dispatch table
    s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("test.cl");
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : 
    nds(NULL), str(s), objectclasstag(0), ioclasstag(1), intclasstag(2),
    boolclasstag(3), stringclasstag(4)
{
    g_clsTablePtr = this;
    enterscope();

    if (cgen_debug) cout << "Building CgenClassTable" << endl;
    install_basic_classes();
    install_classes(classes);
    build_inheritance_tree();
    build_class_tags();

    code();
    exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenClassTable::build_class_tags(){
    m_tagIdList.push_back(std::make_pair(Object, objectclasstag));
    m_tagIdList.push_back(std::make_pair(IO, ioclasstag));
    m_tagIdList.push_back(std::make_pair(Int, intclasstag));
    m_tagIdList.push_back(std::make_pair(Bool, boolclasstag));
    m_tagIdList.push_back(std::make_pair(Str, stringclasstag));

    size_t tag = stringclasstag + 1;    
    //class tags start with stringclasstag + 1
    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNode* node = l->hd();
        if (!(node->basic())){
            m_tagIdList.push_back(std::make_pair(node->name, tag++)); 
        }
    }
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
    if (cgen_debug) cout << "coding global data" << endl;
    code_global_data();

    if (cgen_debug) cout << "choosing gc" << endl;
    code_select_gc();

    if (cgen_debug) cout << "coding constants" << endl;
    code_constants();

    //class ids should be built first
    code_class_nameTab();
    code_class_objTab();
    code_class_dispTab();
    code_protObjs();

    if (cgen_debug) cout << "coding global text" << endl;
    code_global_text();

    code_class_initializers();
    code_class_method_definitions();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


void CgenClassTable::code_class_nameTab(){
    //classname table should be organized by classtag id sequence
    str << CLASSNAMETAB << LABEL;

    std::ostringstream strm;
    //builtin first by id sequences
    Symbol basics[] = {Object, IO, Int, Bool, Str};
    for (size_t i = 0; i < sizeof(basics)/sizeof(basics[0]); ++i){
        strm.str("");strm.clear();
        strm << basics[i];
        if (cgen_debug) cout << "<<< checking builtin class " << strm.str() << endl; 
        StringEntryP entry = stringtable.lookup_string(const_cast<char*>(strm.str().c_str()));
        str << "\t#class name = " << strm.str() << endl;
        str << WORD;
        entry->code_ref(str);
        str << endl;
    }

    //self-defined classes
    for (size_t tagId = stringclasstag + 1; tagId < m_tagIdList.size(); ++tagId){
        for (ClassTagTable::iterator it = m_tagIdList.begin(), itEnd = m_tagIdList.end();
                it != itEnd; ++it){
            if (it->second == tagId){
                strm.clear();strm.str("");
                strm << it->first;
                StringEntryP entry = stringtable.lookup_string(const_cast<char*>(strm.str().c_str()));
                str << "\t#class name = " << strm.str() << endl;
                str << WORD;
                entry->code_ref(str);
                str << endl;
            }
        }
    }
}

void CgenClassTable::code_class_objTab(){
    str << CLASSOBJTAB << LABEL;
    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNode* hdr = l->hd();
        str << WORD << hdr->get_name() << PROTOBJ_SUFFIX << endl;
        str << WORD << hdr->get_name() << CLASSINIT_SUFFIX << endl;
    }
}

void CgenClassTable::code_class_dispTab(){
    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNode* clsNodePtr = l->hd();
        Symbol clsName = clsNodePtr->get_name();

        FeatureNameList feature_list = clsNodePtr->get_feature_list();
        str << clsName << DISPTAB_SUFFIX << LABEL;
        for (FeatureNameList::iterator it = feature_list.begin(), itEnd = feature_list.end();
                it != itEnd; ++it){
            str << WORD << it->first->name << "." << it->second.first << endl;
        }
    }
}


namespace{
    bool does_className_match(const ClassTagTable::value_type& item, Symbol clsName){
        return item.first == clsName;
    }
    
    void new_location_for_symbol(Symbol name, char* reg, ostream& s){
        //allocate space to save the new identifier
        RegAddrInfo& addr = g_varTable[name];
        addr.location = LOC_FP;
        addr.offset = g_current_sp_offset++;

        emit_store(reg, 0, SP, s);
        emit_addiu(SP, SP, -4, s);
    }

    void free_symbol_location(Symbol name, ostream& s){
        if (g_varTable.find(name) != g_varTable.end()){
            RegAddrInfo& addr = g_varTable[name];
            g_current_sp_offset--;
            emit_addiu(SP, SP, 4, s);
        }
    }

    //save register value to symbol location
    void save_reg_in_symbol_location(char* reg, Symbol name, ostream& s){
        if (g_varTable.find(name) != g_varTable.end()){
            RegAddrInfo& info = g_varTable[name];
            if (info.location == LOC_FP){
                s << SW << reg << " " << -1 * info.offset * WORD_SIZE << "(" << FP << ")" << endl;
            }else if (info.location == LOC_ARG){
                s << MOVE << "$a" << info.offset << " " << reg << endl;
            }else if (info.location == LOC_TEMP){
                s << MOVE << "$t" << info.offset << " " << reg << endl;
            }
        }
    }

    void load_reg_from_symbol_location(char* reg, Symbol name, ostream& s){
        if (g_varTable.find(name) != g_varTable.end()){
            RegAddrInfo& info = g_varTable[name];
            if (info.location == LOC_FP){
                s << LW << reg << " " << -1 * info.offset * WORD_SIZE << "(" << FP << ")" << endl;
            }else if (info.location == LOC_ARG){
                s << MOVE << reg << " $a" << info.offset << endl;
            }else if (info.location == LOC_TEMP){
                s << MOVE << reg << " $t" << info.offset << endl;
            }
        }
    }
    
    void load_variable_to_a0(Symbol name, ostream& s){
        CgenNodeP cls = g_clsTablePtr->lookup(self);
        if (!cls){
            assert(!"Bad scope for object class!");
        }
        size_t offset = cls->get_attr_offset(name);
        if (offset == 0){
            if (g_varTable.find(name) != g_varTable.end()){
                load_reg_from_symbol_location(ACC, name, s);
            }else{
                assert(!"name not found in class or varTable!");
            }
        }else{
            s << "\t# <<< load attribute name: " << name << endl;
            emit_load(ACC, offset, SELF, s);
        }
    }
}


void CgenClassTable::code_protObjs(){
    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNode* clsNodePtr = l->hd();
        Symbol clsName = clsNodePtr->get_name();

        ClassTagTable::iterator it = std::find_if(m_tagIdList.begin(), m_tagIdList.end(),
                std::tr1::bind(does_className_match, std::tr1::placeholders::_1, clsName));
        if (it != m_tagIdList.end()){
            str << WORD << "-1" << endl 
                << clsName << PROTOBJ_SUFFIX << LABEL
                << WORD << it->second << endl;

            //We must get all attribute list to determinze its size
            FeatureNameList feature_list = clsNodePtr->get_feature_list(false/*Fetch attributes*/);
            str << WORD << (feature_list.size() + 3) << endl; //size = (-1) + tagId + dipTable + #attrs 
            str << WORD << clsName << DISPTAB_SUFFIX << endl;

            if (clsNodePtr->basic()){
                code_basic_protObj_attrs(clsName);
            }else{
                for (FeatureNameList::iterator it = feature_list.begin(), itEnd = feature_list.end();
                        it != itEnd; ++it){
                    str << WORD;
                    if (it->second.second == Str){
                        StringEntry* entry = stringtable.lookup_string("");
                        entry->code_ref(str);
                    }else if (it->second.second == Int){
                        IntEntry* entry = inttable.lookup_string("0");
                        entry->code_ref(str);
                    }else{
                        str << "0";
                    }
                    str << endl;
                }
            }
        }else{
            if (cgen_debug){
                cout << __FILE__ << __LINE__ << endl;
            }
        }
    }
}


void CgenClassTable::code_basic_protObj_attrs(Symbol name){
    if ((name == Int) || (name == Bool)){
        str << WORD << "0" << endl;
    }else if (name == Str){
        //first attribute ref to int 0 - the default length of string is zero
        IntEntry* entry = inttable.lookup_string("0");
        str << WORD;
        entry->code_ref(str);
        str << endl; 
        str << WORD << "0" << endl;
    }else{
        //no attributes for Object/IO
    }
}


namespace{
    void emit_call_save_active_records(ostream& strm){
        emit_addiu(SP, SP, -12, strm);
        emit_store(FP, 3, SP, strm);
        emit_store(SELF, 2, SP, strm);
        emit_store(RA, 1, SP, strm);
        emit_addiu(FP, SP, 4, strm);
    }

    void emit_init_call_save_active_records(ostream& strm){
        //we need to make sure $s0 contains $a0
        emit_call_save_active_records(strm);
        emit_move(SELF, ACC, strm);
    }

    void emit_call_save_and_return(ostream& strm){
        emit_load(FP, 3, SP, strm);
        emit_load(SELF, 2, SP, strm);
        emit_load(RA, 1, SP, strm);
        emit_addiu(SP, SP, 12, strm);
        emit_return(strm);
    }

    void emit_init_call_save_and_return(ostream& strm){
        //We need to return $a0 for init and store $s0 in it
        emit_move(ACC, SELF, strm);
        emit_call_save_and_return(strm);
    }

    void emit_save_temp_registers(ostream& strm){
        strm << "\t#<< Saving temp registers t1&t2" << endl;
        emit_addiu(SP, SP, -8, strm);
        emit_store(T1, 2, SP, strm);
        emit_store(T2, 1, SP, strm);
    }

    void emit_restore_temp_registers(ostream& strm){
        strm << "\t#>> Restoring temp registers t1&t2" << endl;
        emit_load(T1, 1, SP, strm);
        emit_load(T2, 2, SP, strm);
        emit_addiu(SP, SP, 8, strm);
    }
}

void CgenClassTable::code_class_initializers(){
    std::vector<Symbol> initList;
    initList.push_back(Object);
    initList.push_back(Int);
    initList.push_back(Str);
    initList.push_back(Bool);
    initList.push_back(IO);
    code_basic_class_initializers();

    for(List<CgenNode> *l = nds; l; l = l->tl()){
        CgenNode* node = l->hd();

        enterscope();
        addid(self, node);

        if (!(node->basic()) && (std::find(initList.begin(), initList.end(), node->name) == initList.end())){
            //emit initializer
            str << node->name << "_init" << LABEL;

            str << "\t# Save stack and registers..." << endl;
            emit_init_call_save_active_records(str);
            g_current_sp_offset = 1; //Initially, loc = fp + -1 * 4
            CgenNode* parent = node->get_parentnd();
            if (parent){
                if (cgen_debug)
                    cout << JAL << parent->name << "_init" << endl;
                str << JAL << parent->name << "_init" << endl;
            }else{
                //default parent is object is not specified
                str << JAL << "Object_init" << endl;
            }
            Features fs = node->features;
            for (int i = fs->first(); fs->more(i); i = fs->next(i)){
                attr_class* attrPtr = dynamic_cast<attr_class*>(fs->nth(i));
                if (!attrPtr){
                    continue;
                }else{
                    size_t offset = node->get_attr_offset(attrPtr->name);
                    // using get_attr_offset to decide absolute offset
                    if (typeid(*attrPtr->init) != typeid(no_expr_class)){
                        str << "\t# <<< initialize attribute..." << endl;
                        attrPtr->init->code(str);
                        emit_store(ACC, offset, SELF, str);
                        str << "\t# <<< initialize attribute done..." << endl;
                    }else{
                        if ((attrPtr->type_decl == Str) || (attrPtr->type_decl == Bool) || (attrPtr->type_decl == Int)){
                            //default initialization for str/Bool/Int - using the boxed object and save a pointer
                            // save s0 first
                            emit_push(SELF, str);
                            emit_partial_load_address(ACC, str);
                            str << attrPtr->type_decl << PROTOBJ_SUFFIX << endl;
                            emit_jal("Object.copy", str);
                            str << JAL << attrPtr->type_decl << CLASSINIT_SUFFIX << endl;
                            //retore s0
                            emit_pop(SELF, str);
                            emit_store(ACC, offset, SELF, str);
                        }else{
                            str << "\t# Set zero for " << node->name << ":" << attrPtr->name << endl;
                            emit_store(ZERO, offset, SELF, str);
                        }
                    }
                }
            }

            str << "\t# Restore stack and registers..." << endl;
            emit_init_call_save_and_return(str);
        }

        exitscope();

        initList.push_back(node->name);
    }
}


void CgenClassTable::code_basic_class_initializers(){
    Symbol basics[] = {Object, Int, Bool, IO, Str};
    for (size_t i = 0; i < sizeof(basics)/sizeof(basics[0]); ++i){
        str << basics[i] << "_init" << LABEL;
        emit_init_call_save_active_records(str);
        if (basics[i] != Object){
            emit_jal("Object_init", str);
        }
        emit_init_call_save_and_return(str);
    }
}

void CgenClassTable::code_class_method_definitions(){
    for(List<CgenNode> *l = nds; l; l = l->tl()){

        CgenNode* clsPtr = l->hd();
        if (clsPtr->basic()){
            continue; //builtin methods are ready in runtime
        }
        Features fs = clsPtr->features;
        enterscope();

        addid(self, clsPtr);
        if (cgen_debug) cout << "<<< Saving self type as: " << clsPtr->name << endl;

        for (int i = fs->first(); fs->more(i); i = fs->next(i)){
            enterscope();
            method_class* method = dynamic_cast<method_class*>(fs->nth(i));

            if (method){
                //registers are already assigned at callee side
                str << clsPtr->name << "." << method->name << LABEL;
                str << "\t# Prepare and save registers ..." << endl;
                //next_lable_id = 1;
                size_t numOfArgs = method->formals->len();
                emit_call_save_active_records(str);

                //save formal parameters' address for later reference
                for (size_t i = 0; i < numOfArgs; ++i){
                    formal_class* argInfo = dynamic_cast<formal_class*>(method->formals->nth(i));
                    assert(argInfo);
                    RegAddrInfo& addrInfo = g_varTable[argInfo->name];
                    if (i < 3){
                        //first 3 arguments saved in a1,a2,a3, self in a0
                        addrInfo.location = LOC_ARG;
                        addrInfo.offset = i + 1; 
                    }else{
                        //Those arguments saved in stack below our fp and registers
                        // below fp, 0:ra, -1:s0, -2:oldfp -3:arg3, -4:arg4
                        //  ..., -i:arg(i), ..., -(n-1) : arg(n-1)
                        addrInfo.location = LOC_FP;
                        addrInfo.offset = -1 * i;
                    }
                }

                str << "\t# Generate method body experissions..." << endl;
                method->expr->code(str);

                //exit scope for names
                //names shall be popped directly
                for (size_t i = 0; i < numOfArgs; ++i){
                    formal_class* argInfo = dynamic_cast<formal_class*>(method->formals->nth(i));
                    g_varTable.erase(argInfo->name);
                }
                if (numOfArgs > 3){
                    emit_addiu(SP, SP, -1 * (numOfArgs - 3) * 4, str);
                }

                str << "\t# Save and return ..." << endl;
                emit_call_save_and_return(str);
            }

            exitscope(); //method scope
        }

        exitscope(); //exit class scope
    }
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
   m_ancestorsBuilt = false;
   m_attrMapBuilt = false;
   m_methodMapBuilt = false;
}

//should be called after inheritance tree determined
void CgenNode::build_ancestors(){
    if (m_ancestorsBuilt)
        return;

    CgenNodeP ptr = get_parentnd();
    while (ptr != NULL){
        m_ancestors.push_back(ptr);
        ptr = ptr->get_parentnd();
    }
    //From top to bottom
    std::reverse(m_ancestors.begin(), m_ancestors.end());
    m_ancestorsBuilt = true;
}


namespace{
    bool name_matches(Symbol method_name, FeatureNameList::value_type& method){
        return method.second.first == method_name;
    }

    template<typename T> Symbol get_type(const T& type){assert(!"don't instattiate!"); }
    template<>
    Symbol get_type(const method_class& method){return method.return_type;}
    template<>
    Symbol get_type(const attr_class& attr){return attr.type_decl;}

    template<typename FeatureType>
    void check_and_add_features(FeatureNameList& features_list, Features& features, CgenNodeP clsNode){
        for (int i = features->first(); features->more(i); i = features->next(i)){
            FeatureType* feature = dynamic_cast<FeatureType*>(features->nth(i));
            if (feature){
                //All parent classes already checked due to sorting
                FeatureNameList::iterator itMethod = std::find_if(features_list.begin(), 
                        features_list.end(), std::tr1::bind(name_matches, feature->name, 
                            std::tr1::placeholders::_1));
                if (itMethod == features_list.end()){
                    features_list.push_back(std::make_pair(clsNode, 
                        std::make_pair(feature->name, get_type(*feature))));
                }else{
                    //override by current parent
                    itMethod->first = clsNode;
                }
            }else{
                //No process
            }
        }
    }

}


namespace{
    size_t find_name(Symbol name, AttributeMap& tbl){
        if (tbl.empty()){
            if (cgen_debug){
                cout << "!!! <<< unable to find " << name << " from map... " << endl;
            }
            return 0;
        }
        if (tbl.find(name) == tbl.end()){
            if (cgen_debug){
                cout << "@@@ symbol name " << name << " not in attr/method table" << endl << "<<<<<<";
                for (AttributeMap::iterator it = tbl.begin(), it1 = tbl.end(); it != it1; ++it){
                    cout << it->first << "->" << it->second << ",";
                }
                cout << endl;
            }
            return 0;
        }
        return tbl[name];
    }
}
size_t CgenNode::get_attr_offset(Symbol attr){
       if (!m_attrMapBuilt){
           get_feature_list(false); 
       }
       //if (cgen_debug) cout << "<<<@@@ find attr map " << attr << " from class " << name << endl;
       return find_name(attr, m_attrMap);
}

size_t CgenNode::get_method_offset(Symbol method){
    //if (cgen_debug) cout << "<<< Finding method " << method << endl;
    if (!m_methodMapBuilt){
        get_feature_list(); 
    }
    return find_name(method, m_methodMap);
}

FeatureNameList CgenNode::get_feature_list(bool check_on_method){
    build_ancestors();

    FeatureNameList features_list;
    for (AncestorList::iterator it = m_ancestors.begin(), itEnd = m_ancestors.end();
            it != itEnd; ++it){
        CgenNodeP ancestor = *it;
        //Symbol clsName = ancestor->name;
        if (ancestor && ancestor->features){
            Features cls_features = ancestor->features;
            if (check_on_method){
                check_and_add_features<method_class>(features_list, cls_features, ancestor);
            }else{
                check_and_add_features<attr_class>(features_list, cls_features, ancestor);
            }
        }else{
            if (cgen_debug){
                cout << "Bad ancestor found!" << __LINE__ << endl;
            }
        }
    }
    //add own features finally
    if (check_on_method){
        check_and_add_features<method_class>(features_list, this->features, this);
    }else{
        check_and_add_features<attr_class>(features_list, this->features, this);
    }

    if (cgen_debug){
        cout << "<<< Class " << name << ": number of " << (check_on_method ? "methods:" : "attrs:") << (features_list.size()) << endl;
    }

    //build attribute map for offset calculation
    if ((!m_attrMapBuilt) && (!check_on_method)){
        size_t offset = 3; //classid + size + dispTable
        for (FeatureNameList::iterator it = features_list.begin(), itEnd = features_list.end();
                it != itEnd; ++it){
            m_attrMap[it->second.first] = offset++;
        }
        if (cgen_debug){
            cout << "<<<@@@ attr map for " << name << ":" ;
            for (AttributeMap::iterator it = m_attrMap.begin(), itEnd = m_attrMap.end(); it!= itEnd; ++it){
                cout << it->first << "->" << it->second << ",";
            }
            cout << endl;
        }
        m_attrMapBuilt = true;

    }
    if ((!m_methodMapBuilt) && (check_on_method)){
        size_t offset = 0;
        for (FeatureNameList::iterator it = features_list.begin(), itEnd = features_list.end();
                it != itEnd; ++it){
            m_methodMap[it->second.first] = offset++;
        }
        m_methodMapBuilt = true;
    }
    return features_list;
}

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************


void assign_class::code(ostream &s) {
    s << "\t# Assign begin...,target = " << name << endl;
    expr->code(s);

    CgenNodeP cls = g_clsTablePtr->lookup(self);
    if (!cls){
        assert(!"Bad scope for object class!");
    }
    size_t offset = cls->get_attr_offset(name);
    if (offset == 0){
        if (g_varTable.find(name) != g_varTable.end()){
            RegAddrInfo& info = g_varTable[name];
            if (info.location == LOC_FP){
                s << SW << ACC << " " << -1 * info.offset * WORD_SIZE << "(" << FP << ")" << endl;
            }else if (info.location == LOC_ARG){
                s << MOVE << "$a" << info.offset << " " << ACC << endl;
            }else if (info.location == LOC_TEMP){
                s << MOVE << "$t" << info.offset << " " << ACC << endl;
            }
        }else{
            assert(!"Name lookup error!");
            return;
        }
    }else{
        emit_store(ACC, offset, SELF, s);
    }

    s << "\t# Assign end..." << endl;
}


namespace{
    void dispatch_common(Expression expr, Symbol name, Expressions actual, Symbol obj_type, ostream& s){    
        int numOfArgRegs = actual->len();
        numOfArgRegs = numOfArgRegs > 3 ? 3 : numOfArgRegs;

        //parent arguments' registers needs to be saved to be referenced later
        s << "\t#@ dispatch " << name << ": save arg registers..." << endl;
        emit_addiu(SP, SP, -4 - 4 * numOfArgRegs, s);

        for (int i = 0; i < numOfArgRegs; ++i){
            s << SW << "$a" << (i + 1) << " " << 4*(numOfArgRegs + 1 - i) << "($sp)" << endl;
        }
        emit_store(SELF, 1, SP, s);

        s << "\t#@ evaluate actual parameters and save into registers/stack" << endl;
        CgenNodeP classPtr = g_clsTablePtr->lookup(obj_type);
        if (!classPtr){
            assert(!"undefined class found!");
            return;
        }
        if (cgen_debug) cout << "Dispatch object class is:" << classPtr->name << endl;

        //identify method formal signature first
        Formals formals = NULL;
        CgenNodeP clsPtr = classPtr;
        while(formals == NULL){
            for (int i = clsPtr->features->first(); clsPtr->features->more(i); i = clsPtr->features->next(i)){
                method_class* method = dynamic_cast<method_class*>(clsPtr->features->nth(i));
                if (method && (method->name == name)){
                    //found
                    formals = method->formals;
                }
            }
            if (formals == NULL){
                //find in parent
                clsPtr = clsPtr->get_parentnd();
            }
        }        
        
        VarAddrTable backupTable;
        //formal parameter generation and save into stack/registers
        if (actual->len() > 0){
            //allocate locations for arguments
            for (int i = actual->len() - 1; i >= 0; --i){
                formal_class* formal = dynamic_cast<formal_class*>(formals->nth(i));

                //check for name conflicts
                if (g_varTable.find(formal->name) == g_varTable.end()){
                    s << "\t# <<< add new name " << formal->name << " to scope now..." << endl;
                }else{
                    //save old address info since the place would be overwritten???
                    s << "\t# <<< old name " << formal->name << " added to backup store and restore to t1..." << endl;
                    backupTable[formal->name] = g_varTable[formal->name];
                    if (i > 3){
                        //address will be overritten on stack!
                        RegAddrInfo& info = g_varTable[formal->name];
                        assert(info.location == LOC_FP);
                        load_reg_from_symbol_location(T1, formal->name, s);

                        std::ostringstream strm;
                        strm << "_hidden_" << formal->name;
                        Symbol backupName = idtable.add_string(strdup(strm.str().c_str()));
                        new_location_for_symbol(backupName, T1, s);
                    }else{
                        //just backup is okay, using new registers while old already saved
                    }
                }

                //save addr info 
                RegAddrInfo& info = g_varTable[formal->name];
                if (i < 3){
                    //save result from acc to registers a1/a2/a3
                    s << MOVE << "$a" << (i+1) <<  " " << ACC << endl;
                    info.location = LOC_ARG;
                    info.offset = (i+1);
                }else{
                    //save in stack, but won't be popped out until done 
                    emit_push(ACC, s);
                    info.location = LOC_FP;
                    info.offset = g_current_sp_offset++;
                }
            }
            
            //evaluation actual parameters and place in location
            for (int i = 0; i < actual->len(); ++i){
                //The arg name might be referenced within the function, so
                // read the formal name first
                formal_class* formal = dynamic_cast<formal_class*>(formals->nth(i));

                s << "\t#<<< " << name << " evaluate argument " << (i+1) << endl;
                assign_class* assign = dynamic_cast<assign_class*>(actual->nth(i));
                actual->nth(i)->code(s);

                //TODO:set formal parameter's value to appropriate location
            }
        }

        //set new a0 since self can only change here!
        s << "\t#<<< " << name << " - evaluate and set new $s0 ..." << endl;
        expr->code(s);
        emit_move(SELF, ACC, s);

        s << "\t#<<< " << name << " - do the call ..." << endl;
        //call function
        size_t method_offset = clsPtr->get_method_offset(name);
        if (cgen_debug) cout << "get method offset for:" << name << " from class:" << clsPtr->name << ",offset=" << method_offset << endl;

        int dispatch_call = next_lable_id++;    
        emit_bne(SELF, ZERO, dispatch_call, s);

        s << "\t# Abort now..." << endl;
        emit_load_string(ACC,stringtable.lookup_string("test.cl"), s);
        emit_load_imm(T1, 1, s);
        emit_jal("_dispatch_abort", s);

        emit_label_def(dispatch_call, s);
        emit_load(T1, 2, SELF, s); // offset 2 = dispTab
        emit_load(T1, method_offset, T1, s);
        emit_jalr(T1, s);

        //need to restore self here
        s<< "\t#<<< " << name << " done, restore registers ..." << endl;
        emit_load(SELF, 1, SP, s);

        for (int i = 0; i < numOfArgRegs; ++i){
            s << LW << "$a" << (i+1) << " " << (numOfArgRegs + 1 - i)*4 << "($sp)" << endl;
        }
        emit_addiu(SP, SP, (numOfArgRegs + 1) * 4, s);

        //restore allocated names for arguments
        // pop used stack space and remove from varTable
        // The name itself will be popped
        if (formals->len() > 3){
            s << "\t#@ " << name << ": used stack space rewinding..." << endl;
            emit_addiu(SP, SP, (formals->len() - 3) * 4, s);
        }

        if (formals->len() > 0){
            for (int i = formals->len() - 1; i >= 0; --i){
                Symbol symName = (dynamic_cast<formal_class*>(formals->nth(i)))->name;
                if (backupTable.find(symName) != backupTable.end()){
                    s << "\t# >>> name: " <<  symName << " addr restored now!" << endl;
                    g_varTable[symName] = backupTable[symName];

                    if (i > 3){
                        //load and restore address value into t1 and save into location
                        std::ostringstream strm;
                        strm << "_hidden_" << symName;
                        Symbol hidden_name = idtable.lookup_string(const_cast<char*>(strm.str().c_str()));
                        RegAddrInfo& info = g_varTable[hidden_name]; //must on stack
                        emit_load(T1, -1 * info.offset, FP, s);
                        save_reg_in_symbol_location(T1, symName, s);

                        //erase hidden_name
                        free_symbol_location(hidden_name, s);
                        g_varTable.erase(hidden_name);
                    }else{
                        //will restore to registers
                    }
                }else{
                    s << "\t# >>> name: " <<  symName << " out of scope now!" << endl;
                    g_varTable.erase(symName);
                }
            }
        }
        
    }
}


void static_dispatch_class::code(ostream &s) {
    dispatch_common(expr, name, actual, type_name, s);
}

void dispatch_class::code(ostream &s) {
    Symbol type = expr->get_type();
    if (expr->get_type() == SELF_TYPE){
        CgenNode* cls = g_clsTablePtr->lookup(self);
        if (!cls){
            assert(!"self type not found!");
        }
        type = cls->name;
    }
    if (cgen_debug){
        cout << "dispatch on expr type:" << type << endl;
    }
    dispatch_common(expr, name, actual, type, s);
}

void cond_class::code(ostream &s) {
    pred->code(s);
    //emit_save_temp_registers(s);
    emit_move(T1, ACC, s);
    emit_load(T1, first_attr_offset, T1, s);

    int true_label = next_lable_id++;
    int false_label = next_lable_id++;
    emit_bne(T1, ZERO, true_label, s);
    emit_branch(false_label, s);

    int joint_label = next_lable_id++;
    emit_label_def(true_label,s);
    then_exp->code(s);
    emit_move(T1, ACC, s);
    emit_branch(joint_label, s);

    emit_label_def(false_label, s);
    else_exp->code(s);
    emit_move(T1, ACC, s);
    emit_branch(joint_label, s);

    emit_label_def(joint_label, s);
    //emit_restore_temp_registers(s);
}

void loop_class::code(ostream &s) {
    //emit_save_temp_registers(s);

    int begin_label = next_lable_id++;
    emit_label_def(begin_label, s);

    pred->code(s);
    emit_move(T1, ACC, s);
    emit_load(T1, first_attr_offset, T1, s);
    
    int true_label = next_lable_id++;
    int false_label = next_lable_id++;
    emit_bne(T1, ZERO, true_label, s);
    emit_branch(false_label, s);

    emit_label_def(true_label, s);
    body->code(s);
    emit_branch(begin_label, s);

    //do nothing for false
    emit_label_def(false_label, s);
    //emit_restore_temp_registers(s);
}

void typcase_class::code(ostream &s) {
    s << "\t#$$ typecase begin..." << endl;

    expr->code(s);
    emit_move(T1, ACC, s);
    emit_bne(T1, ZERO, next_lable_id, s);

    emit_load_string(ACC,stringtable.lookup_string("test.cl"), s);
    emit_load_imm(T1, 1, s);
    emit_jal("_case_abort2", s);

    emit_label_def(next_lable_id++, s);
    //select the cloest branch
    int case_branch = -1;
    branch_class* branch = NULL;
    CgenNode* clsPtr = g_clsTablePtr->lookup(expr->get_type());
    while(case_branch == -1){
        for (int i = cases->first(); cases->more(i); i = cases->next(i)){
            branch = dynamic_cast<branch_class*>(cases->nth(i));
            if (clsPtr->name == branch->type_decl){
                case_branch = i;
            }
        }

        if (case_branch == -1){
            clsPtr = clsPtr->get_parentnd();
        }
    }
    
    //allocate new location with value from T1
    new_location_for_symbol(branch->name, T1, s);
    branch->expr->code(s);
    free_symbol_location(branch->name, s);

    s << "\t#$$ typecase end..." << endl;
}

void block_class::code(ostream &s) {
    for (int i = body->first(); body->more(i); i = body->next(i)){
        body->nth(i)->code(s);
    }
    //last one in ACC already
}

void let_class::code(ostream &s) {
    g_clsTablePtr->enterscope();

    s << "\t# let statement begin..." << endl;
    s << "\t#<<< let init ..." << endl;
    init->code(s);
    if (init->get_type() == NULL){
        if (cgen_debug)
            cout << "<<< let statement, init type:" << (init->get_type() ? init->get_type() : No_type) << endl;
        //Load zero here!
        emit_move(ACC, ZERO, s);
    }
    new_location_for_symbol(identifier, ACC, s);
    s << "\t#<<< let body ..." << endl;
    body->code(s);

    s << "\t#<<< pop out the symbol ..." << endl;
    free_symbol_location(identifier, s);

    s << "\t# let statement end..." << endl;
    g_clsTablePtr->exitscope();
}


#define __GEN_ARITH_CODE__(name, com) \
    s << "\t#" << name << " begin: evaluate t1 -> $a0 and push to stack" << endl;\
    e1->code(s);\
    emit_jal("Object.copy", s);\
    emit_push(ACC, s);\
    s << "\t#>> evaluate e2 to $t2" << endl;\
    e2->code(s);\
    emit_move(T2, ACC, s);\
    s << "\t#>> restore e1 to $t1 and do $t1 + $t2" << endl;\
    emit_pop(ACC, s);\
    emit_load(T1, first_attr_offset, ACC, s); \
    emit_load(T2, first_attr_offset, T2, s);\
    com(T1, T1, T2, s);\
    s << "\t#>> store value to $a0 for return" << endl;\
    emit_store(T1, first_attr_offset, ACC, s);\
    s << "\t#" << name << " end!" << endl;
    
void plus_class::code(ostream &s) {
    __GEN_ARITH_CODE__("Plus", emit_add);
}

void sub_class::code(ostream &s) {
    __GEN_ARITH_CODE__("Sub", emit_sub);
}

void mul_class::code(ostream &s) {
    __GEN_ARITH_CODE__("Mul", emit_mul);
}

void divide_class::code(ostream &s) {
    __GEN_ARITH_CODE__("Div", emit_div);
}

void neg_class::code(ostream &s) {
    s << "\t@@@ << Neg begin..." << endl;
    e1->code(s);
    emit_jal("Object.copy", s);
    emit_load(T1, first_attr_offset, ACC, s);
    emit_load_imm(V0, -1, s);
    emit_mul(T1, T1, V0, s);
    emit_store(T1, first_attr_offset, ACC, s);
    s << "\t@@@ << Neg end..." << endl;
}


#define __GEN_COMP_CODE(name, comp)   \
    s << "#" << name << " Begin..." << endl;\
    e1->code(s);\
    s << "# << save t1&t2..." << endl;\
    emit_addiu(SP, SP, -8, s);\
    emit_store(T1, 2, SP, s);\
    emit_store(T2, 1, SP, s);\
    emit_load(T1, first_attr_offset, ACC, s);\
    e2->code(s);\
    emit_load(T2, first_attr_offset, ACC, s);\
    int true_label = next_lable_id++;\
    int false_label = next_lable_id++;\
    int joint_label = next_lable_id++;\
    comp(T1, ACC, true_label, s);\
    emit_label_def(true_label, s);\
    emit_load_bool(ACC, truebool, s);\
    emit_branch(joint_label, s);\
    emit_label_def(false_label, s);\
    emit_load_bool(ACC, falsebool, s);\
    emit_branch(joint_label, s);\
    emit_label_def(joint_label, s);\
    emit_load(T1, 1, SP, s);\
    emit_load(T2, 2, SP, s);\
    emit_addiu(SP, SP, 8, s);\
    s << "#" << name << " end..." << endl;

void lt_class::code(ostream &s) {
    __GEN_COMP_CODE("LT", emit_blt);
}


void eq_class::code(ostream &s) {
    s << "#@@@ Equal check begin..." << endl;
    e1->code(s);
    //emit_save_temp_registers(s);
    emit_move(T1, ACC, s);
    e2->code(s);
    emit_move(T2, ACC, s);

    //now e1 in T1, e2 in a0
    int true_label = next_lable_id++;
    int false_label = next_lable_id++;
    emit_beq(T1, T2, true_label, s);
    emit_beq(T1, ZERO, false_label, s);
    emit_beq(T2, ZERO, false_label, s);
    if (e1->get_type() == Int || e1->get_type() == Bool){
        s << "\t#<< compare Int/Bool contents..." << endl;
        emit_load(T1, first_attr_offset, T1, s);
        emit_load(T2, first_attr_offset, T2, s);
        emit_beq(T1, T2, true_label, s);
        emit_branch(false_label, s);
    }else if(e1->get_type() == Str){
        s << "\t#<< compare String contents..." << endl;
        emit_load(V0, first_attr_offset, T1, s);
        emit_load(V1, first_attr_offset, T2, s);
        emit_beq(V0, V1, true_label, s);
        emit_load(T1, first_attr_offset + 1, T1, s);
        emit_load(T2, first_attr_offset + 1, T2, s);
        emit_beq(T1, T2, true_label, s);
        emit_branch(false_label, s);
    }

    int joint_label = next_lable_id++;
    emit_label_def(true_label, s);
    emit_load_bool(ACC, truebool, s);
    emit_branch(joint_label, s);

    emit_label_def(false_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_branch(joint_label, s);

    emit_label_def(joint_label, s);
    //emit_restore_temp_registers(s);
    s << "#@@@ Equal check end..." << endl;
}

void leq_class::code(ostream &s) {
    __GEN_COMP_CODE("LEQ", emit_bleq);
}

void comp_class::code(ostream &s) {
    s << "\t#@@@ Not begin..." << endl;
    e1->code(s);
    //emit_save_temp_registers(s);
    emit_load(T1, first_attr_offset, ACC, s);
    emit_load_imm(T2, 1, s);
    emit_sub(T1, T2, T1, s); //1->0, 0->1
    emit_store(T1, first_attr_offset, ACC, s);

    //emit_restore_temp_registers(s);
    s << "\t#@@@ Not end..." << endl;
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}


void new__class::code(ostream &s) {
    s << "\t#<<< new object begin " << endl;
    CgenNode* clsPtr = g_clsTablePtr->lookup(type_name);
    emit_partial_load_address(ACC, s);
    s << clsPtr->name << PROTOBJ_SUFFIX << endl;
    emit_jal("Object.copy", s);
    s << JAL << type_name << CLASSINIT_SUFFIX << endl;
    s << "\t#>>> new object end " << endl;
}

void isvoid_class::code(ostream &s) {
    int true_label = next_lable_id++;
    int false_label = next_lable_id++;
    
    e1->code(s);
    emit_move(T1, ACC, s);
    emit_bne(T1, ZERO, true_label, s);
    emit_branch(false_label, s);

    int joint_label = next_lable_id++;
    emit_label_def(true_label, s);
    emit_load_bool(ACC, truebool, s);
    emit_branch(joint_label, s);

    emit_label_def(false_label, s);
    emit_load_bool(ACC, falsebool, s);
    emit_branch(joint_label, s);

    emit_label_def(joint_label, s);
}

void no_expr_class::code(ostream &s) {
    //do nothing 
}

void object_class::code(ostream &s) {
    s << "\t#@@@ object begin..." << endl;
    if (name == self){
        s << "\t#<<< move s0 to a0 for self ..." << endl;
        emit_move(ACC, SELF, s);
    }else{
        load_variable_to_a0(name, s);
    }
    s << "\t#@@@ object done..." << endl;
}

