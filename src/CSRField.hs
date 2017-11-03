-- This module exists to provide a separate namespace for CSR fields (as opposed
-- to CSRs), to prevent conflicts via qualified imports.
module CSRField where

data CSRField = MXL | Extensions | -- misa
                SXL | UXL | TSR | TW | TVM | MXR | SUM | MPRV | XS | FS | MPP |
                SPP | MPIE | SPIE | UPIE | MIE | SIE | UIE | SD | -- mstatus
                MTVecBase | MTVecMode | -- mtvec
                MEDeleg | -- medeleg
                MIDeleg | -- mideleg
                MEIP | SEIP | UEIP | MTIP | STIP | UTIP | MSIP | SSIP | USIP | -- mip
                MEIE | SEIE | UEIE | MTIE | STIE | UTIE | MSIE | SSIE | USIE | -- mie
                MCycle | -- mcycle
                MInstRet | -- minstret
                MHPM | MIR | MTM | MCY | -- mcounteren
                MScratch | -- mscratch
                MEPC | -- mepc
                MCauseInterrupt | MCauseCode | -- mcause
                MTVal -- mtval
  deriving (Ord, Eq, Show)
