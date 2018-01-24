module mct_mod

use m_attrvect ,only: AttrVect
use m_attrvect ,only: avect_init =>init
use m_attrvect ,only: avect_clean => clean
use m_attrvect ,only: avect_zero => zero
use m_attrvect ,only: avect_lsize => lsize
use m_attrvect ,only: avect_indexIA => indexIA
use m_attrvect ,only: avect_indexRA => indexRA

use m_GeneralGrid ,only: gGrid => GeneralGrid
use m_GeneralGrid ,only: gGrid_init => init
use m_GeneralGrid ,only: gGrid_clean => clean
use m_GeneralGrid ,only: gGrid_dims => dims
use m_GeneralGrid ,only: gGrid_lsize => lsize
use m_GeneralGrid ,only: gGrid_indexIA => indexIA
use m_GeneralGrid ,only: gGrid_indexRA => indexRA

use m_Transfer ,only: mct_send => Send
use m_Transfer ,only: mct_recv => Recv


use m_GlobalSegMap ,only: gsMap => GlobalSegMap
use m_GlobalSegMap ,only: gsMap_init => init
use m_GlobalSegMap ,only: gsMap_clean => clean

use m_Rearranger ,only: rearr => Rearranger
use m_Rearranger ,only: rearr_init => init
use m_Rearranger ,only: rearr_clean => clean

use m_Router ,only: router => Router
use m_Router ,only: router_init => init


implicit none
    public :: noo

contains 

subroutine noo()

    write(*,*) 'noops'

end subroutine noo

end module mct_mod
