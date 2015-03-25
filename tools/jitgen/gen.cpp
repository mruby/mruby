#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/User.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/IRPrintingPasses.h>
#include <llvm/IR/AssemblyAnnotationWriter.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/IR/ValueSymbolTable.h>
#include <unordered_map>
#include <iostream>

#undef NDEBUG
#include <cassert>


using namespace llvm;

std::unordered_map<std::string, int> symbolMap;
unsigned symbolIndex;

Instruction *getSymTbl(Function *func, LLVMContext& context) {
  ValueSymbolTable &funSymTbl = func->getValueSymbolTable();
  Value *symTblLoadInst = funSymTbl.lookup("__symtbl__");
  if(!symTblLoadInst) {
    Value *index0 = ConstantInt::get(Type::getInt32Ty(context), 0, false);
    std::vector<Value *> indices {index0, index0};

    Value *opCtxArg = &(*func->getArgumentList().begin());

    Instruction *symTblGepInst = GetElementPtrInst::CreateInBounds(opCtxArg, indices, "");
    symTblLoadInst = new LoadInst(symTblGepInst, "__symtbl__");

    func->getEntryBlock().getInstList().push_front(cast<Instruction>(symTblLoadInst));
    func->getEntryBlock().getInstList().push_front(symTblGepInst);
  }

  return cast<Instruction>(symTblLoadInst);
}

Instruction *getSymbol(Function *func, Instruction *inst, Type *type, BasicBlock *bb, Module *mod, LLVMContext& context, unsigned index) {
  Instruction *symTblLoadInst = getSymTbl(func, context);
  Value *indexVal = ConstantInt::get(mod->getDataLayout()->getIntPtrType(context), index, false);
  Instruction *gepInst = GetElementPtrInst::CreateInBounds(symTblLoadInst, indexVal, "");
  CastInst* castInst = new BitCastInst(gepInst, type, "");
  bb->getInstList().insert(inst, castInst);
  bb->getInstList().insert(castInst, gepInst);

  return castInst;
}

bool isOpFunc(const std::string& name) {
  std::string opPrefix = "op_";
  return !name.compare(0, opPrefix.size(), opPrefix);
}

bool isOpFunc(Function *func) {
  return isOpFunc(func->getName().str());
}


int main(int argc, const char **argv)
{
  LLVMContext context;
  SMDiagnostic error;

  if(argc < 3) {
    puts("Usage: gen module.bc outputdir");
    exit(1);
  }

  auto mod = parseIRFile(argv[1], error, context);
  std::string outputDir(argv[2]);

  std::vector<Function *> removeFuncs;

  ValueSymbolTable &symTbl = mod->getValueSymbolTable();
  int symIndex = 0;

  for(ValueSymbolTable::iterator symIter = symTbl.begin(); symIter != symTbl.end(); ++symIter) {
    std::string sym = symIter->getKey().str();
    Value *val = symIter->getValue();

    std::vector<CallInst *> callInsts;
    std::vector<User *> gvUsers;

    for(User* user: val->users()) {
      if(isa<CallInst>(user)) {
        callInsts.push_back(cast<CallInst>(user));
      }
    }

    if(isa<GlobalVariable>(val)) {
      for(User* user: val->users()) {
        gvUsers.push_back(user);
      }
    }

    for(CallInst *callInst: callInsts) {
      BasicBlock *bb = callInst->getParent();
      Function *func = bb->getParent();

      std::string funcName = func->getName().str();

      if(isOpFunc(funcName)) {
        std::cerr << "=========================" << std::endl;
        std::cerr << funcName << std::endl;
        std::cerr << "=========================" << std::endl;

        std::cerr << symIter->getKey().str() << std::endl;

        Value *calledValue = callInst->getCalledValue();

        if(isa<MemCpyInst>(callInst)) {
        } else {
          Instruction *castInst = getSymbol(func, callInst, calledValue->getType(),
                                            bb, &*mod, context, symIndex);

          callInst->setCalledFunction(castInst);


          if(symbolMap.find(sym) == symbolMap.end()) {
            symbolMap[sym] = symIndex++;
          }
        }
        //val->replaceAllUsesWith(castInst);
        //use->replaceUsesOfWith(replaceInst);
      }
    }


    for(User *user: gvUsers) {
      Instruction *inst = nullptr;

      if(isa<Instruction>(user)) {
        inst = cast<Instruction>(user);
      } /* else if(isa<ConstantExpr>(user)) {
        inst = cast<ConstantExpr>(user)->getAsInstruction();
      }*/

      if(inst) {
        BasicBlock *bb = inst->getParent();
        Function *func = bb->getParent();

        std::string funcName = func->getName().str();

        if(isOpFunc(funcName)) {
          std::cerr << "=========================" << std::endl;
          std::cerr << funcName << std::endl;
          std::cerr << "=========================" << std::endl;

          std::cerr << symIter->getKey().str() << std::endl;

          Instruction *castInst = getSymbol(func, inst, val->getType(),
                                            bb, &*mod, context, symIndex);

          user->replaceUsesOfWith(val, castInst);

          if(symbolMap.find(sym) == symbolMap.end()) {
            symbolMap[sym] = symIndex++;
          }

        }
      } else {
      }
    }
  }

  for(Module::iterator funcIter = mod->begin(); funcIter != mod->end(); ++funcIter) {
    std::string funcName = funcIter->getName().str();

    if(isOpFunc(funcName)) {
      std::cerr << "=========================" << std::endl;
      std::cerr << funcName << std::endl;
      std::cerr << "=========================" << std::endl;

      Module *opMod = new Module(funcName, context);
      opMod->setDataLayout(mod->getDataLayout());
      opMod->setTargetTriple(mod->getTargetTriple());
      opMod->setModuleInlineAsm(mod->getModuleInlineAsm());

      for (Function::iterator bbIter = funcIter->begin(), bbEnd = funcIter->end(); bbIter != bbEnd; ++bbIter) {
        if(isOpFunc(funcIter)) {
          for (BasicBlock::iterator instIter = bbIter->begin(), instEnd = bbIter->end(); instIter != instEnd; ++instIter) {
            if(MemCpyInst *memCpyInst = dyn_cast<MemCpyInst>(instIter)) {
              Function *calledFunc = memCpyInst->getCalledFunction();
              ArrayRef<Type *> params = makeArrayRef(calledFunc->getFunctionType()->param_begin(),
                                                     calledFunc->getFunctionType()->param_end() - 2);

              memCpyInst->setCalledFunction(Intrinsic::getDeclaration(opMod, Intrinsic::memcpy, params));
            }

            if(User *user = dyn_cast<User>(instIter)) {
              for(unsigned i = 0; i < user->getNumOperands(); i++) {
                Value *operand = user->getOperand(i);
                if(ConstantExpr *constExpr = dyn_cast<ConstantExpr>(operand)) {
                  for(unsigned j = 0; j < constExpr->getNumOperands(); j++) {
                    Value *ceOperand = cast<User>(constExpr)->getOperand(j);
                    if(isa<GlobalVariable>(ceOperand)) {
                      std::string name = ceOperand->getName().str();
                      std::cerr << "op" << name << std::endl;
                      auto indexEntry = symbolMap.find(name);
                      unsigned index;

                      if(indexEntry != symbolMap.end()) {
                        index = indexEntry->second;
                      } else {
                        index = symIndex++;
                        symbolMap[name] = index;
                      }

                      Instruction *castInst = getSymbol(funcIter, instIter, operand->getType(),
                                                        bbIter, &*mod, context, index);
                      cast<User>(user)->setOperand(i, castInst);
                    }
                  }
                }
              }
            }
          }
        }
      }



      ValueToValueMapTy map;
      Function *clonedFunc = CloneFunction(funcIter, map,
                                            true);


      opMod->getFunctionList().push_back(clonedFunc);

      verifyModule(*opMod, &llvm::outs());

      std::error_code ec;
      std::string name = funcName + ".ll";
      //std::string name = opName + ".bc";
      std::string sep = "";
      if(*outputDir.rbegin() != '/') {
        sep = "/";
      }
      raw_fd_ostream os(outputDir + sep + name.c_str(), ec, sys::fs::F_None);

      //WriteBitcodeToFile(module, os);
      AssemblyAnnotationWriter w;
      opMod->print(os, &w);
      os.close();
      std::cerr << "Written " << ec << std::endl;
      std::cerr << std::endl << std::endl;
    }
  }

  std::cout << "void **jit_sym_tbl = {" << std::endl;
  for(auto symIter = symbolMap.begin(); symIter != symbolMap.end(); ++symIter) {
    std::cout << "  " << symIter->first << "," << std::endl;
  }
  std::cout << "};" << std::endl;


  return 0;
}
