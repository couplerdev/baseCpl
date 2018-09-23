module mct_mod

use m_MCTWorld ,only: mct_world_init => init
use m_attrvect ,only: AttrVect
use m_attrvect ,only: avect_init =>init
use m_attrvect ,only: avect_clean => clean
use m_attrvect ,only: avect_zero => zero
use m_attrvect ,only: avect_copy => copy
use m_attrvect ,only: avect_lsize => lsize
use m_attrvect ,only: avect_indexIA => indexIA
use m_attrvect ,only: avect_indexRA => indexRA
use m_attrvect ,only: avect_importRA => importRAttr
use m_attrvect ,only: avect_importIattr => importIattr
use m_attrvect ,only: avect_exportIattr => exportIattr
use m_attrvect ,only: avect_importRattr => importRattr
use m_attrvect ,only: avect_exportRattr => exportRattr
use m_attrvect ,only: avect_getIList => getIList
use m_attrvect ,only: avect_getRList => getRList
use m_attrvect ,only: avect_exportIList2c => exportIListToChar
use m_attrvect ,only: avect_exportRList2c => exportRListToChar

use m_attrvect ,only: avect_nRattr => nRattr

use m_GeneralGrid ,only: gGrid => GeneralGrid
use m_GeneralGrid ,only: gGrid_init => init
use m_GeneralGrid ,only: gGrid_clean => clean
use m_GeneralGrid ,only: gGrid_dims => dims
use m_GeneralGrid ,only: gGrid_lsize => lsize
use m_GeneralGrid ,only: gGrid_indexIA => indexIA
use m_GeneralGrid ,only: gGrid_indexRA => indexRA

use m_GeneralGrid ,only: gGrid_importRAttr => importRAttr
use m_GeneralGrid ,only: gGrid_importIAttr => importIAttr

use m_Transfer ,only: mct_send => Send
use m_Transfer ,only: mct_recv => Recv


use m_GlobalSegMap ,only: gsMap => GlobalSegMap
use m_GlobalSegMap ,only: gsMap_init => init
use m_GlobalSegMap ,only: gsMap_clean => clean
use m_GlobalSegMap ,only: gsMap_gsize  => gsize
use m_GlobalSegMap ,only: gsMap_activepes  => active_pes
use m_GlobalSegMap ,only: gsMap_lsize  => lsize
use m_GlobalSegMap ,only: gsMap_copy  => copy
use m_GlobalSegMap ,only: gsMap_nlseg  => nlseg

use m_GlobalSegMap ,only: gsMap_order  => OrderedPoints

use m_GlobalSegMapComms ,only: gsmap_bcast => bcast


use m_Rearranger ,only: gRearr => Rearranger
use m_Rearranger ,only: rearr_init => init
use m_Rearranger ,only: rearr_clean => clean
use m_Rearranger ,only: rearrange => Rearrange

use m_Router ,only: router => Router
use m_Router ,only: router_init => init


use m_SparseMatrix, only : SparseMatrix
use m_SparseMatrix, only : sMat_init => init
use m_SparseMatrix, only : sMat_clean => clean
use m_SparseMatrix, only : sMat_vecinit => vecinit
use m_SparseMatrix, only : sMat_importGRowInd => importGlobalRowIndices
use m_SparseMatrix, only : sMat_importGColInd => importGlobalColumnIndices
use m_SparseMatrix, only : sMat_importMatrixElts => importMatrixElements
use m_SparseMatrix, only : sMat_indexIA => indexIA
use m_SparseMatrix, only : sMat_indexRA => indexRA
use m_SparseMatrix, only : sMat_lsize => lsize
use m_SparseMatrix, only : sMat_nrows => nRows
use m_SParseMatrix, only : sMat_ncols => nCols
use m_SparseMatrix, only : sMat_GNumEl => GlobalNumElements
use m_SparseMatrixPlus, only : SparseMatrixPlus
use m_SparseMatrixPlus, only : sMatPlus_init => init
use m_SparseMatrixPlus, only : sMatPlus_clean => clean
use m_SparseMatrixPlus, only : sMat_Xonly => Xonly ! Decompose matrix by row



!---Matrix-Vector multiply methods
use m_MatAttrVectMul, only: sMatAvect_Mult => sMatAvMult

implicit none
    public :: noo

contains 

subroutine noo()

    write(*,*) 'noops'

end subroutine noo

end module mct_mod
