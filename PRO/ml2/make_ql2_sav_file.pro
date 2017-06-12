.reset_session

.compile ql2.pro
.compile cimage__define.pro*
.compile cimwin__define.pro*
.compile cconfigs__define.pro
.compile cdfilter__define.pro
.compile cfitshedit__define.pro
.compile cplotwin__define.pro*
.compile cpntmode__define.pro
.compile cpolling__define.pro
.compile cprint__define.pro

resolve_all, class='cimage'
resolve_all, class='cimwin'
resolve_all, class='cconfigs'
resolve_all, class='cdfilter'
resolve_all, class='cfitshedit'
resolve_all, class='cplotwin'
resolve_all, class='cpntmode'
resolve_all, class='cpolling'
resolve_all, class='cprint'
resolve_all, ql2

save, /routines, filename='ql2.sav'
