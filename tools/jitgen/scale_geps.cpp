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
#include <iostream>

#undef NDEBUG

using namespace llvm;

void scaleGeps(Function *func, Module *mod) {
  for (Function::iterator bbIter = func->begin(), bbEnd = func->end(); bbIter != bbEnd; ++bbIter) {
  for (BasicBlock::iterator instIter = bbIter->begin(), instEnd = bbIter->end(); instIter != instEnd; ++instIter) {

    if(GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(instIter)) {
      for(unsigned i = 0; i < gepInst->getNumOperands(); i++) {
        Value *operand = gepInst->getOperand(i);
        if(ConstantInt *constInt = dyn_cast<ConstantInt>(operand)) {
          uint64_t v = constInt->getZExtValue();
          uint64_t normV = constInt->getZExtValue() & 0xFF0000;
          if(normV == 0xAB0000 ||
             normV == 0xBC0000 ||
             normV == 0xCD0000 ||
             normV == 0xDE0000 ) {
            Type *ty = cast<PointerType>(gepInst->getPointerOperandType())->getElementType();
            gepInst->dump();
            ty->dump();
            uint64_t size = mod->getDataLayout()->getTypeAllocSize(ty);
            uint64_t add = v - normV;

            /*std::cout << std::hex << (size) << std::endl;
            std::cout << std::hex << (size << 8) << std::endl;
            std::cout << std::hex << (v / size) << std::endl;
            std::cout << std::hex << (v / size + (size << 8)) << std::endl;*/

            assert(size <= 0xFF);
            assert(add <= 0xFF);
            uint64_t newV = (normV + (size << 8)) / size + add;
            gepInst->setOperand(i, ConstantInt::get(constInt->getType(), newV));

            std::cout << v << "=>" << newV << "(" << size << ")" << std::endl;

            //for(User *user: g)
          }
        }
      }
    }
  }
  }
}


int main(int argc, const char **argv)
{
  LLVMContext context;
  SMDiagnostic error;

  if(argc < 3) {
    puts("Usage: scale_geps infile outfile");
    exit(1);
  }

  auto mod = parseIRFile(argv[1], error, context);
  for(Function &func: mod->functions()) {
    scaleGeps(&func, &*mod);
  }

  verifyModule(*mod, &llvm::outs());

  std::error_code ec;
  std::string name = argv[1];
  raw_fd_ostream os(name.c_str(), ec, sys::fs::F_None);

  //WriteBitcodeToFile(module, os);
  AssemblyAnnotationWriter w;
  mod->print(os, &w);
  os.close();
}
