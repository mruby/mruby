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

bool isOpFunc(const std::string& name) {
  std::string prefix = "op_";
  bool llvmGenerated = name.at(name.length() - 1) == '_';
  return !name.compare(0, prefix.size(), prefix) && !llvmGenerated;
}

bool isDummySymbol(const std::string& name) {
  std::string prefix = "__mrb_jit_";
  return !name.compare(0, prefix.size(), prefix);
}

bool isIntrinsicSymbol(const std::string& name) {
  std::string prefix = "llvm";
  return !name.compare(0, prefix.size(), prefix);
}

bool isArgSymbol(const std::string& name) {
  return name == "A" ||
         name == "B" ||
         name == "Bx" ||
         name == "b" ||
         name == "sBx" ||
         name == "Ax" ||
         name == "C" ||
         name == "c";
}

bool isOpFunc(Function *func) {
  return isOpFunc(func->getName().str());
}

void cloneGlobalVariable(Module &m, GlobalVariable *gV, ValueToValueMapTy& map)
{
  GlobalVariable *newGV = m.getGlobalVariable(gV->getName());
  if(!newGV) {
    GlobalValue::LinkageTypes linkage;
    Constant *init;
    if(gV->hasName()) {
      if(isArgSymbol(gV->getName().str())) {
        linkage = gV->getLinkage();
        init = gV->getInitializer();
      } else {
        linkage = GlobalVariable::ExternalLinkage;
        init = nullptr;
      }
    } else if(gV->hasInitializer()) {
      assert(0);
      // untested

      linkage = gV->getLinkage();
      init = gV->getInitializer();
      if(ConstantArray *ac = dyn_cast<ConstantArray>(init)) {
        std::vector<Constant *> ops;
        for(Use& use: ac->operands()) {
          ops.push_back(cast<Constant>(&use));
        }
        init = ConstantArray::get(ac->getType(), ArrayRef<Constant *>(ops));
      } else {
      }
    }
    newGV = new GlobalVariable(m,
                          gV->getType()->getElementType(),
                          gV->isConstant(), linkage,
                          init, gV->getName(),
                          (GlobalVariable*) nullptr,
                          gV->getThreadLocalMode(),
                          gV->getType()->getAddressSpace());
    newGV->copyAttributesFrom(gV);
  }
  map[gV] = newGV;
}

void cloneGlobalVariables(Module &mod, User *user, ValueToValueMapTy& map) {
  for(unsigned i = 0; i < user->getNumOperands(); i++) {
    Value *operand = user->getOperand(i);
    if(GlobalVariable *gV = dyn_cast<GlobalVariable>(operand)) {
      cloneGlobalVariable(mod, gV, map);
    } else if(User *userOperand = dyn_cast<User>(operand)) {
      cloneGlobalVariables(mod, userOperand, map);
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

  for(Module::iterator funcIter = mod->begin(); funcIter != mod->end(); ++funcIter) {
    std::string funcName = funcIter->getName().str();

    if(isOpFunc(funcName)) {
      std::cerr << "=========================" << std::endl;
      std::cerr << funcName << std::endl;
      std::cerr << "=========================" << std::endl;

      Module *opMod = new Module(funcName, context);
      ValueToValueMapTy map;

      for (Function::iterator bbIter = funcIter->begin(), bbEnd = funcIter->end(); bbIter != bbEnd; ++bbIter) {
        for (BasicBlock::iterator instIter = bbIter->begin(), instEnd = bbIter->end(); instIter != instEnd; ++instIter) {
          cloneGlobalVariables(*opMod, instIter, map);
        }
      }

      opMod->setDataLayout(mod->getDataLayout());
      opMod->setTargetTriple(mod->getTargetTriple());
      opMod->setModuleInlineAsm(mod->getModuleInlineAsm());

      Function *clonedFunc = CloneFunction(funcIter, map,
                                            true);

      opMod->getFunctionList().push_back(clonedFunc);

      std::vector<Instruction *> removeInsts;

      for (Function::iterator bbIter = clonedFunc->begin(), bbEnd = clonedFunc->end(); bbIter != bbEnd; ++bbIter) {
          for (BasicBlock::iterator instIter = bbIter->begin(), instEnd = bbIter->end(); instIter != instEnd; ++instIter) {
            if(MemCpyInst *memCpyInst = dyn_cast<MemCpyInst>(instIter)) {
              Function *calledFunc = memCpyInst->getCalledFunction();
              ArrayRef<Type *> params = makeArrayRef(calledFunc->getFunctionType()->param_begin(),
                                                     calledFunc->getFunctionType()->param_end() - 2);

              memCpyInst->setCalledFunction(Intrinsic::getDeclaration(opMod, Intrinsic::memcpy, params));
            } else if(CallInst *callInst = dyn_cast<CallInst>(instIter)) {
              Value *calledValue = callInst->getCalledValue();
              std::string calledName = calledValue->getName().str();

              if(isDummySymbol(calledName)) {
                removeInsts.push_back(callInst);
              } else if(/*isIntrinsicSymbol(calledName) && */ isa<Function>(calledValue)) {
                Function *calledFunc = cast<Function>(calledValue);
                Function *newCalledFunc = opMod->getFunction(calledName);
                if(!newCalledFunc) {
                  FunctionType *funcType = calledFunc->getFunctionType();
                  newCalledFunc = Function::Create(funcType, GlobalValue::ExternalLinkage, calledName, opMod);
                }
                assert(newCalledFunc);
                callInst->setCalledFunction(newCalledFunc);
              }
            }
          }
      }

      for(Instruction *inst: removeInsts) {
        inst->eraseFromParent();
      }

      Value *ctxArg = &(*clonedFunc->getArgumentList().begin());
      CallInst *ci = CallInst::Create(clonedFunc, ctxArg, "", clonedFunc->back().getTerminator());

      clonedFunc->setLinkage(GlobalValue::LinkageTypes::ExternalLinkage);

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

      delete opMod;
      opMod = nullptr;
    }
  }

  return 0;
}
