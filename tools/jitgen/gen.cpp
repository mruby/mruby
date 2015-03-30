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

  Value *index0 = ConstantInt::get(Type::getInt32Ty(context), 0, false);
  Value *index1 = ConstantInt::get(Type::getInt32Ty(context), 1, false);
  Value *indexVal = ConstantInt::get(Type::getInt32Ty(context), index, false);
  //Value *indexVal = ConstantInt::get(mod->getDataLayout()->getIntPtrType(context), index, false);

  Value *opCtxArg = &(*func->getArgumentList().begin());
  int nElems = cast<StructType>(cast<PointerType>(opCtxArg->getType())->getElementType())->getNumElements();

  std::cout << nElems << std::endl;

  Value *nElemsVal = ConstantInt::get(Type::getInt32Ty(context), nElems - 1, false);
  std::vector<Value *> indices0 {index0, nElemsVal, indexVal};
  Instruction *gepInst0 = GetElementPtrInst::Create(opCtxArg, indices0);

  //std::vector<Value *> indices {index0, indexVal};
  //Instruction *gepInst1 = GetElementPtrInst::Create(castInst0, indexVal);
  //
  CastInst* castInst1 = new BitCastInst(gepInst0, type->getPointerTo());
  Instruction *loadInst = new LoadInst(castInst1);

  bb->getInstList().insert(inst, gepInst0);
  bb->getInstList().insertAfter(gepInst0, castInst1);
  bb->getInstList().insertAfter(castInst1, loadInst);

 /* bb->getInstList().insertAfter(gepInst0, castInst0);
  bb->getInstList().insertAfter(castInst0, gepInst1);
  bb->getInstList().insertAfter(gepInst1, castInst1);
  bb->getInstList().insertAfter(castInst1, loadInst);*/


  return loadInst;

  /*
  Instruction *symTblLoadInst = getSymTbl(func, context);
  Value *indexVal = ConstantInt::get(mod->getDataLayout()->getIntPtrType(context), index, false);
  Instruction *gepInst = GetElementPtrInst::CreateInBounds(symTblLoadInst, indexVal, "");
  CastInst* castInst = new BitCastInst(gepInst, type, "");
  bb->getInstList().insert(inst, castInst);
  bb->getInstList().insert(castInst, gepInst);
  return castInst;
  */

}

bool isOpFunc(const std::string& name) {
  std::string prefix = "op_";
  return !name.compare(0, prefix.size(), prefix);
}

bool isDummySymbol(const std::string& name) {
  std::string prefix = "__";
  return !name.compare(0, prefix.size(), prefix);
}

bool isOpFunc(Function *func) {
  return isOpFunc(func->getName().str());
}

unsigned getSymbolIndex(const std::string& name) {
  auto indexEntry = symbolMap.find(name);
  int symIndex = -1;

  if(isDummySymbol(name)) {
    assert(0 && "dummy symbol");
  }

  if(indexEntry != symbolMap.end()) {
    symIndex = indexEntry->second;
  } else {
    symIndex = symbolIndex++;
    symbolMap[name] = symIndex;
  }
  return symIndex;
}

void findGepUsers(User *parentUser, Module *mod, LLVMContext &context, std::vector<User *>& chain) {
  chain.push_back(parentUser);
  for(User *user: parentUser->users()) {
    if(GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(user)) {
      uint64_t size = mod->getDataLayout()->getTypeAllocSize(cast<PointerType>(gepInst->getType())->getElementType());
      std::cout << size << std::endl;
      gepInst->dump();
      Value *magicConst = ConstantInt::get(context, APInt(32, 0xABCDEF / size, false));
      gepInst->replaceUsesOfWith(parentUser, magicConst);
      //assert(0);
    } else {
      findGepUsers(user, mod, context, chain);
    }
  }
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

  std::vector<Instruction *> removeInsts;

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

          std::vector<User *> chain;
    for(CallInst *callInst: callInsts) {
      BasicBlock *bb = callInst->getParent();
      Function *func = bb->getParent();

      std::string funcName = func->getName().str();

      if(isDummySymbol(sym)) {
        removeInsts.push_back(callInst);
      } else if(isOpFunc(funcName)) {
        std::cerr << "=========================" << std::endl;
        std::cerr << funcName << std::endl;
        std::cerr << "=========================" << std::endl;


        Value *calledValue = callInst->getCalledValue();

        if(isa<MemCpyInst>(callInst)) {
        } else {
          unsigned symIndex = getSymbolIndex(sym);
          std::cerr << symIter->getKey().str() << " " << symIndex << std::endl;

          Instruction *castInst = getSymbol(func, callInst, calledValue->getType(),
                                            bb, &*mod, context, symIndex);

          callInst->setCalledFunction(castInst);
        }
        //val->replaceAllUsesWith(castInst);
        //use->replaceUsesOfWith(replaceInst);
      }
    }


      for(User *user: chain) {
        if(Instruction *inst = dyn_cast<Instruction>(user)) {
          inst->removeFromParent();
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
                                            bb, &*mod, context, getSymbolIndex(sym));

          user->replaceUsesOfWith(val, castInst);
        }
      } else {
      }
    }
  }

  for(Instruction *inst: removeInsts) {
    inst->removeFromParent();
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

            if(GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(instIter)) {
              for(unsigned i = 0; i < gepInst->getNumOperands(); i++) {
                Value *operand = gepInst->getOperand(i);
                if(ConstantInt *constInt = dyn_cast<ConstantInt>(operand)) {
                  uint64_t v = constInt->getZExtValue();
                  if(v == 0xABCD00 ||
                     v == 0xBCDE00 ||
                     v == 0xCDEF00) {
                    uint64_t size = mod->getDataLayout()->getTypeAllocSize(cast<PointerType>(gepInst->getType())->getElementType());
                    gepInst->setOperand(i, ConstantInt::get(constInt->getType(), v / size));
                  }
                }
              }
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
                        index = symbolIndex++;
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


      Value *ctxArg = &(*clonedFunc->getArgumentList().begin());
      CallInst *ci = CallInst::Create(clonedFunc, ctxArg, "", clonedFunc->back().getTerminator());

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

  std::vector<std::string> syms(symbolMap.size());
  for(auto symIter = symbolMap.begin(); symIter != symbolMap.end(); ++symIter) {
    syms[symIter->second] = symIter->first;
  }

  std::cout << "%w{";
  for(std::string sym: syms) {
    std::cout << "  " << sym;
  }
  std::cout << "}" << std::endl;

  return 0;
}
