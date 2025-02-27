
//

#ifndef JITJAM_PREPROCESS_ENGINE_H
#define JITJAM_PREPROCESS_ENGINE_H

#include "../../../sljit_interop/jit_types.h"
#include "../../../config/jitjam_config.h"
#include "../constraints/constraint_value.h"
#include "../var.h"
#include "../block.h"


using namespace jt;

class PreProcEngine {
    friend class Block;

    JitJamConfig *config;
    // indexed-into by BlockId
    std::vector<Block> blocks = {};
    std::vector<Var> variables = {};

    int var_id_counter = 0;
    int alias_id_counter = 0;
    int block_id_counter = 0;

    PreProcEngine(JitJamConfig *config);

    VariableId unique_var_id();
    AliasId unique_alias_id();
    BlockId unique_block_id();
    Var declare_variable(const VariableTypeId &var_type_id);
    Var shadow_alias(const Var &shadowed_var);

public:

    BlockId add_block(const Block &block);
    Block &borrow_block(const BlockId &block_id);
    void compile();
    void compile_block(Block &block);
    void fold_consts();
};

#endif //JITJAM_PREPROCESS_ENGINE_H
