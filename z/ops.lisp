;;;; ops.lisp

(in-package #:specops.z)

(specop-z a       zformat-rx-a  #x005A)

(specop-z ad      zformat-rx-a  #x006A)

(specop-z adb     zformat-rxe   #xED1A)

(specop-z adbr    zformat-rre   #xB31A)

(specop-z adr     zformat-rr    #x002A)

(specop-z adtr    zformat-rrf-a #xB3D2)

(specop-z adtra   zformat-rrf-a #xB3D2)

(specop-z ae      zformat-rx-a  #x007A)

(specop-z aeb     zformat-rxe   #xED0A)

(specop-z aebr    zformat-rre   #xB30A)

(specop-z aer     zformat-rr    #x003A)

(specop-z afi     zformat-ril-a #x0C29)

(specop-z ag      zformat-rxy-a #xE308)

(specop-z agf     zformat-rxy-a #xE318)

(specop-z agfi    zformat-ril-a #x0C28)

(specop-z agfr    zformat-rre   #xB918)

(specop-z agh     zformat-rxy-a #xE338)

(specop-z aghi    zformat-ri-a  #x0A7B)

(specop-z aghik   zformat-rie-d #xECD9)

(specop-z agr     zformat-rre   #xB908)

(specop-z agrk    zformat-rrf-a #xB9E8)

(specop-z agsi    zformat-siy   #xEB7A)

(specop-z ah      zformat-rx-a  #x004A)

(specop-z ahhhr   zformat-rrf-a #xB9C8)

(specop-z ahhlr   zformat-rrf-a #xB9D8)

(specop-z ahi     zformat-ri-a  #x0A7A)

(specop-z ahik    zformat-rie-d #xECD8)

(specop-z ahy     zformat-rxy-a #xE37A)

(specop-z aih     zformat-ril-a #x0CC8)

(specop-z al      zformat-rx-a  #x005E)

(specop-z alc     zformat-rxy-a #xE398)

(specop-z alcg    zformat-rxy-a #xE388)

(specop-z alcgr   zformat-rre   #xB988)

(specop-z alcr    zformat-rre   #xB998)

(specop-z alfi    zformat-ril-a #x0C2B)

(specop-z alg     zformat-rxy-a #xE30A)

(specop-z algf    zformat-rxy-a #xE31A)

(specop-z algfi   zformat-ril-a #x0C2A)

(specop-z algfr   zformat-rre   #xB91A)

(specop-z alghsik zformat-rie-d #xECDB)

(specop-z algr    zformat-rre   #xB90A)

(specop-z algrk   zformat-rrf-a #xB9EA)

(specop-z algsi   zformat-siy   #xEB7E)

(specop-z alhhhr  zformat-rrf-a #xB9CA)

(specop-z alhhlr  zformat-rrf-a #xB9DA)

(specop-z alhsik  zformat-rie-d #xECDA)

(specop-z alr     zformat-rr    #x001E)

(specop-z alrk    zformat-rrf-a #xB9FA)

(specop-z alsi    zformat-siy   #xEB6E)

(specop-z alsih   zformat-ril-a #x0CCA)

(specop-z alsihn  zformat-ril-a #x0CCB)

(specop-z aly     zformat-rxy-a #xE35E)

(specop-z ap      zformat-ss-b  #x00FA)

(specop-z ar      zformat-rr    #x001A)

(specop-z ark     zformat-rrf-a #xB9F8)

(specop-z asi     zformat-siy   #xEB6A)

(specop-z au      zformat-rx-a  #x007E)

(specop-z aur     zformat-rr    #x003E)

(specop-z aw      zformat-rx-a  #x006E)

(specop-z awr     zformat-rr    #x002E)

(specop-z axbr    zformat-rre   #xB34A)

(specop-z axr     zformat-rr    #x0036)

(specop-z axtr    zformat-rrf-a #xB3DA)

(specop-z axtra   zformat-rrf-a #xB3DA)

(specop-z ay      zformat-rxy-a #xE35A)

(specop-z bakr    zformat-rre   #xB240)

(specop-z bal     zformat-rx-a  #x0045)

(specop-z balr    zformat-rr    #x0005)

(specop-z bas     zformat-rx-a  #x004D)

(specop-z basr    zformat-rr    #x000D)

(specop-z bassm   zformat-rr    #x000C)

(specop-z bc      zformat-rx-b  #x0047)

(specop-z bcr     zformat-rr    #x0007)

(specop-z bct     zformat-rx-a  #x0046)

(specop-z bctg    zformat-rxy-a #xE346)

(specop-z bctgr   zformat-rre   #xB946)

(specop-z bctr    zformat-rr    #x0006)

(specop-z bic     zformat-rxy-b #xE347)

(specop-z bpp     zformat-smi   #x00C7)

(specop-z bprp    zformat-mii   #x00C5)

(specop-z bras    zformat-ri-b  #x0A75)

(specop-z brasl   zformat-ril-b #x0C05)

(specop-z brc     zformat-ri-c  #x0A74)

(specop-z brcl    zformat-ril-c #x0C04)

(specop-z brct    zformat-ri-b  #x0A76)

(specop-z brctg   zformat-ri-b  #x0A77)

(specop-z brcth   zformat-ril-b #x0CC6)

(specop-z brxh    zformat-rsi   #x0084)

(specop-z brxhg   zformat-rie-e #xEC44)

(specop-z brxle   zformat-rsi   #x0085)

(specop-z brxlg   zformat-rie-e #xEC45)

(specop-z bsa     zformat-rre   #xB25A)

(specop-z bsg     zformat-rre   #xB258)

(specop-z bsm     zformat-rr    #x000B)

(specop-z bxh     zformat-rs-a  #x0086)

(specop-z bxhg    zformat-rsy-a #xEB44)

(specop-z bxle    zformat-rs-a  #x0087)

(specop-z bxleg   zformat-rsy-a #xEB45)

(specop-z c       zformat-rx-a  #x0059)

(specop-z cd      zformat-rx-a  #x0069)

(specop-z cdb     zformat-rxe   #xED19)

(specop-z cdbr    zformat-rre   #xB319)

(specop-z cdfbr   zformat-rre   #xB395)

(specop-z cdfbra  zformat-rrf-e #xB395)

(specop-z cdfr    zformat-rre   #xB3B5)

(specop-z cdftr   zformat-rre   #xB951)

(specop-z cdgbr   zformat-rre   #xB3A5)

(specop-z cdgbra  zformat-rrf-e #xB3A5)

(specop-z cdgr    zformat-rre   #xB3C5)

(specop-z cdgtr   zformat-rre   #xB3F1)

(specop-z cdgtra  zformat-rrf-e #xB3F1)

(specop-z cdlfbr  zformat-rrf-e #xB391)

(specop-z cdlftr  zformat-rrf-e #xB953)

(specop-z cdlgbr  zformat-rrf-e #xB3A1)

(specop-z cdlgtr  zformat-rrf-e #xB952)

(specop-z cdpt    zformat-rsl-b #xEDAE)

(specop-z cdr     zformat-rr    #x0029)

(specop-z cds     zformat-rs-a  #x00BB)

(specop-z cdsg    zformat-rsy-a #xEB3E)

(specop-z cdstr   zformat-rre   #xB3F3)

(specop-z cdsy    zformat-rsy-a #xEB31)

(specop-z cdtr    zformat-rre   #xB3E4)

(specop-z cdutr   zformat-rre   #xB3F2)

(specop-z cdzt    zformat-rsl-b #xEDAA)

(specop-z ce      zformat-rx-a  #x0079)

(specop-z ceb     zformat-rxe   #xED09)

(specop-z cebr    zformat-rre   #xB309)

(specop-z cedtr   zformat-rre   #xB3F4)

(specop-z cefbr   zformat-rre   #xB394)

(specop-z cefbra  zformat-rrf-e #xB394)

(specop-z cefr    zformat-rre   #xB3B4)

(specop-z cegbr   zformat-rre   #xB3A4)

(specop-z cegbra  zformat-rrf-e #xB3A4)

(specop-z cegr    zformat-rre   #xB3C4)

(specop-z celfbr  zformat-rrf-e #xB390)

(specop-z celgbr  zformat-rrf-e #xB3A0)

(specop-z cer     zformat-rr    #x0039)

(specop-z cextr   zformat-rre   #xB3FC)

(specop-z cfc     zformat-s     #xB21A)

(specop-z cfdbr   zformat-rrf-e #xB399)

(specop-z cfdbra  zformat-rrf-e #xB399)

(specop-z cfdr    zformat-rrf-e #xB3B9)

(specop-z cfdtr   zformat-rrf-e #xB941)

(specop-z cfebr   zformat-rrf-e #xB398)

(specop-z cfebra  zformat-rrf-e #xB398)

(specop-z cfer    zformat-rrf-e #xB3B8)

(specop-z cfi     zformat-ril-a #x0C2D)

(specop-z cfxbr   zformat-rrf-e #xB39A)

(specop-z cfxbra  zformat-rrf-e #xB39A)

(specop-z cfxr    zformat-rrf-e #xB3BA)

(specop-z cfxtr   zformat-rrf-e #xB949)

(specop-z cg      zformat-rxy-a #xE320)

(specop-z cgdbr   zformat-rrf-e #xB3A9)

(specop-z cgdbra  zformat-rrf-e #xB3A9)

(specop-z cgdr    zformat-rrf-e #xB3C9)

(specop-z cgdtr   zformat-rrf-e #xB3E1)

(specop-z cgdtra  zformat-rrf-e #xB3E1)

(specop-z cgebr   zformat-rrf-e #xB3A8)

(specop-z cgebra  zformat-rrf-e #xB3A8)

(specop-z cger    zformat-rrf-e #xB3C8)

(specop-z cgf     zformat-rxy-a #xE330)

(specop-z cgfi    zformat-ril-a #x0C2C)

(specop-z cgfr    zformat-rre   #xB930)

(specop-z cgfrl   zformat-ril-b #x0C6C)

(specop-z cgh     zformat-rxy-a #xE334)

(specop-z cghi    zformat-ri-a  #x0A7F)

(specop-z cghrl   zformat-ril-b #x0C64)

(specop-z cghsi   zformat-sil   #xE558)

(specop-z cgib    zformat-ris   #xECFC)

(specop-z cgij    zformat-rie-c #xEC7C)

(specop-z cgit    zformat-rie-a #xEC70)

(specop-z cgr     zformat-rre   #xB920)

(specop-z cgrb    zformat-rrs   #xECE4)

(specop-z cgrj    zformat-rie-b #xEC64)

(specop-z cgrl    zformat-ril-b #x0C68)

(specop-z cgrt    zformat-rrf-c #xB960)

(specop-z cgxbr   zformat-rrf-e #xB3AA)

(specop-z cgxbra  zformat-rrf-e #xB3AA)

(specop-z cgxr    zformat-rrf-e #xB3CA)

(specop-z cgxtr   zformat-rrf-e #xB3E9)

(specop-z cgxtra  zformat-rrf-e #xB3E9)

(specop-z ch      zformat-rx-a  #x0049)

(specop-z chf     zformat-rxy-a #xE3CD)

(specop-z chhr    zformat-rre   #xB9CD)

(specop-z chhsi   zformat-sil   #xE554)

(specop-z chi     zformat-ri-a  #x0A7E)

(specop-z chlr    zformat-rre   #xB9DD)

(specop-z chrl    zformat-ril-b #x0C65)

(specop-z chsi    zformat-sil   #xE55C)

(specop-z chy     zformat-rxy-a #xE379)

(specop-z cib     zformat-ris   #xECFE)

(specop-z cih     zformat-ril-a #x0CCD)

(specop-z cij     zformat-rie-c #xEC7E)

(specop-z cit     zformat-rie-a #xEC72)

(specop-z cksm    zformat-rre   #xB241)

(specop-z cl      zformat-rx-a  #x0055)

(specop-z clc     zformat-ss-a  #x00D5)

(specop-z clcl    zformat-rr    #x000F)

(specop-z clcle   zformat-rs-a  #x00A9)

(specop-z clclu   zformat-rsy-a #xEB8F)

(specop-z clfdbr  zformat-rrf-e #xB39D)

(specop-z clfdtr  zformat-rrf-e #xB943)

(specop-z clfebr  zformat-rrf-e #xB39C)

(specop-z clfhsi  zformat-sil   #xE55D)

(specop-z clfi    zformat-ril-a #x0C2F)

(specop-z clfit   zformat-rie-a #xEC73)

(specop-z clfxbr  zformat-rrf-e #xB39E)

(specop-z clfxtr  zformat-rrf-e #xB94B)

(specop-z clg     zformat-rxy-a #xE321)

(specop-z clgdbr  zformat-rrf-e #xB3AD)

(specop-z clgdtr  zformat-rrf-e #xB942)

(specop-z clgebr  zformat-rrf-e #xB3AC)

(specop-z clgf    zformat-rxy-a #xE331)

(specop-z clgfi   zformat-ril-a #x0C2E)

(specop-z clgfr   zformat-rre   #xB931)

(specop-z clgfrl  zformat-ril-b #x0C6E)

(specop-z clghrl  zformat-ril-b #x0C66)

(specop-z clghsi  zformat-sil   #xE559)

(specop-z clgib   zformat-ris   #xECFD)

(specop-z clgij   zformat-rie-c #xEC7D)

(specop-z clgit   zformat-rie-a #xEC71)

(specop-z clgr    zformat-rre   #xB921)

(specop-z clgrb   zformat-rrs   #xECE5)

(specop-z clgrj   zformat-rie-b #xEC65)

(specop-z clgrl   zformat-ril-b #x0C6A)

(specop-z clgrt   zformat-rrf-c #xB961)

(specop-z clgt    zformat-rsy-b #xEB2B)

(specop-z clgxbr  zformat-rrf-e #xB3AE)

(specop-z clgxtr  zformat-rrf-e #xB94A)

(specop-z clhf    zformat-rxy-a #xE3CF)

(specop-z clhhr   zformat-rre   #xB9CF)

(specop-z clhhsi  zformat-sil   #xE555)

(specop-z clhlr   zformat-rre   #xB9DF)

(specop-z clhrl   zformat-ril-b #x0C67)

(specop-z cli     zformat-si    #x0095)

(specop-z clib    zformat-ris   #xECFF)

(specop-z clih    zformat-ril-a #x0CCF)

(specop-z clij    zformat-rie-c #xEC7F)

(specop-z cliy    zformat-siy   #xEB55)

(specop-z clm     zformat-rs-b  #x00BD)

(specop-z clmh    zformat-rsy-b #xEB20)

(specop-z clmy    zformat-rsy-b #xEB21)

(specop-z clr     zformat-rr    #x0015)

(specop-z clrb    zformat-rrs   #xECF7)

(specop-z clrj    zformat-rie-b #xEC77)

(specop-z clrl    zformat-ril-b #x0C6F)

(specop-z clrt    zformat-rrf-c #xB973)

(specop-z clst    zformat-rre   #xB25D)

(specop-z clt     zformat-rsy-b #xEB23)

(specop-z cly     zformat-rxy-a #xE355)

(specop-z cmpsc   zformat-rre   #xB263)

(specop-z cp      zformat-ss-b  #x00F9)

(specop-z cpdt    zformat-rsl-b #xEDAC)

(specop-z cpsdr   zformat-rrf-b #xB372)

(specop-z cpxt    zformat-rsl-b #xEDAD)

(specop-z cpya    zformat-rre   #xB24D)

(specop-z cr      zformat-rr    #x0019)

(specop-z crb     zformat-rrs   #xECF6)

(specop-z crdte   zformat-rrf-b #xB98F)

(specop-z crj     zformat-rie-b #xEC76)

(specop-z crl     zformat-ril-b #x0C6D)

(specop-z crt     zformat-rrf-c #xB972)

(specop-z cs      zformat-rs-a  #x00BA)

(specop-z csch    zformat-s     #xB230)

(specop-z csdtr   zformat-rrf-d #xB3E3)

(specop-z csg     zformat-rsy-a #xEB30)

(specop-z csp     zformat-rre   #xB250)

(specop-z cspg    zformat-rre   #xB98A)

(specop-z csst    zformat-ssf   #x0C82)

(specop-z csxtr   zformat-rrf-d #xB3EB)

(specop-z csy     zformat-rsy-a #xEB14)

(specop-z cu12    zformat-rrf-c #xB2A7)

(specop-z cu14    zformat-rrf-c #xB9B0)

(specop-z cu21    zformat-rrf-c #xB2A6)

(specop-z cu24    zformat-rrf-c #xB9B1)

(specop-z cu41    zformat-rre   #xB9B2)

(specop-z cu42    zformat-rre   #xB9B3)

(specop-z cudtr   zformat-rre   #xB3E2)

(specop-z cuse    zformat-rre   #xB257)

(specop-z cutfu   zformat-rrf-c #xB2A7)

(specop-z cuutf   zformat-rrf-c #xB2A6)

(specop-z cuxtr   zformat-rre   #xB3EA)

(specop-z cvb     zformat-rx-a  #x004F)

(specop-z cvbg    zformat-rxy-a #xE30E)

(specop-z cvby    zformat-rxy-a #xE306)

(specop-z cvd     zformat-rx-a  #x004E)

(specop-z cvdg    zformat-rxy-a #xE32E)

(specop-z cvdy    zformat-rxy-a #xE326)

(specop-z cxbr    zformat-rre   #xB349)

(specop-z cxfbr   zformat-rre   #xB396)

(specop-z cxfbra  zformat-rrf-e #xB396)

(specop-z cxfr    zformat-rre   #xB3B6)

(specop-z cxftr   zformat-rre   #xB959)

(specop-z cxgbr   zformat-rre   #xB3A6)

(specop-z cxgbra  zformat-rrf-e #xB3A6)

(specop-z cxgr    zformat-rre   #xB3C6)

(specop-z cxgtr   zformat-rre   #xB3F9)

(specop-z cxgtra  zformat-rrf-e #xB3F9)

(specop-z cxlfbr  zformat-rrf-e #xB392)

(specop-z cxlftr  zformat-rrf-e #xB95B)

(specop-z cxlgbr  zformat-rrf-e #xB3A2)

(specop-z cxlgtr  zformat-rrf-e #xB95A)

(specop-z cxpt    zformat-rsl-b #xEDAF)

(specop-z cxr     zformat-rre   #xB369)

(specop-z cxstr   zformat-rre   #xB3FB)

(specop-z cxtr    zformat-rre   #xB3EC)

(specop-z cxutr   zformat-rre   #xB3FA)

(specop-z cxzt    zformat-rsl-b #xEDAB)

(specop-z cy      zformat-rxy-a #xE359)

(specop-z czdt    zformat-rsl-b #xEDA8)

(specop-z czxt    zformat-rsl-b #xEDA9)

(specop-z d       zformat-rx-a  #x005D)

(specop-z dd      zformat-rx-a  #x006D)

(specop-z ddb     zformat-rxe   #xED1D)

(specop-z ddbr    zformat-rre   #xB31D)

(specop-z ddr     zformat-rr    #x002D)

(specop-z ddtr    zformat-rrf-a #xB3D1)

(specop-z ddtra   zformat-rrf-a #xB3D1)

(specop-z de      zformat-rx-a  #x007D)

(specop-z deb     zformat-rxe   #xED0D)

(specop-z debr    zformat-rre   #xB30D)

(specop-z der     zformat-rr    #x003D)

(specop-z dfltcc  zformat-rrf-a #xB939)

(specop-z didbr   zformat-rrf-b #xB35B)

(specop-z diebr   zformat-rrf-b #xB353)

(specop-z dl      zformat-rxy-a #xE397)

(specop-z dlg     zformat-rxy-a #xE387)

(specop-z dlgr    zformat-rre   #xB987)

(specop-z dlr     zformat-rre   #xB997)

(specop-z dp      zformat-ss-b  #x00FD)

(specop-z dr      zformat-rr    #x001D)

(specop-z dsg     zformat-rxy-a #xE30D)

(specop-z dsgf    zformat-rxy-a #xE31D)

(specop-z dsgfr   zformat-rre   #xB91D)

(specop-z dsgr    zformat-rre   #xB90D)

(specop-z dxbr    zformat-rre   #xB34D)

(specop-z dxr     zformat-rre   #xB22D)

(specop-z dxtr    zformat-rrf-a #xB3D9)

(specop-z dxtra   zformat-rrf-a #xB3D9)

(specop-z ear     zformat-rre   #xB24F)

(specop-z ecag    zformat-rsy-a #xEB4C)

(specop-z ectg    zformat-ssf   #x0C81)

(specop-z ed      zformat-ss-a  #x00DE)

(specop-z edmk    zformat-ss-a  #x00DF)

(specop-z eedtr   zformat-rre   #xB3E5)

(specop-z eextr   zformat-rre   #xB3ED)

(specop-z efpc    zformat-rre   #xB38C)

(specop-z epair   zformat-rre   #xB99A)

(specop-z epar    zformat-rre   #xB226)

(specop-z epsw    zformat-rre   #xB98D)

(specop-z ereg    zformat-rre   #xB249)

(specop-z eregg   zformat-rre   #xB90E)

(specop-z esair   zformat-rre   #xB99B)

(specop-z esar    zformat-rre   #xB227)

(specop-z esdtr   zformat-rre   #xB3E7)

(specop-z esea    zformat-rre   #xB99D)

(specop-z esta    zformat-rre   #xB24A)

(specop-z esxtr   zformat-rre   #xB3EF)

(specop-z etnd    zformat-rre   #xB2EC)

(specop-z ex      zformat-rx-a  #x0044)

(specop-z exrl    zformat-ril-b #x0C60)

(specop-z fidbr   zformat-rrf-e #xB35F)

(specop-z fidbra  zformat-rrf-e #xB35F)

(specop-z fidr    zformat-rre   #xB37F)

(specop-z fidtr   zformat-rrf-e #xB3D7)

(specop-z fiebr   zformat-rrf-e #xB357)

(specop-z fiebra  zformat-rrf-e #xB357)

(specop-z fier    zformat-rre   #xB377)

(specop-z fixbr   zformat-rrf-e #xB347)

(specop-z fixbra  zformat-rrf-e #xB347)

(specop-z fixr    zformat-rre   #xB367)

(specop-z fixtr   zformat-rrf-e #xB3DF)

(specop-z flogr   zformat-rre   #xB983)

(specop-z hdr     zformat-rr    #x0024)

(specop-z her     zformat-rr    #x0034)

(specop-z hsch    zformat-s     #xB231)

(specop-z iac     zformat-rre   #xB224)

(specop-z ic      zformat-rx-a  #x0043)

(specop-z icm     zformat-rs-b  #x00BF)

(specop-z icmh    zformat-rsy-b #xEB80)

(specop-z icmy    zformat-rsy-b #xEB81)

(specop-z icy     zformat-rxy-a #xE373)

(specop-z idte    zformat-rrf-b #xB98E)

(specop-z iedtr   zformat-rrf-b #xB3F6)

(specop-z iextr   zformat-rrf-b #xB3FE)

(specop-z iihf    zformat-ril-a #x0C08)

(specop-z iihh    zformat-ri-a  #x0A50)

(specop-z iihl    zformat-ri-a  #x0A51)

(specop-z iilf    zformat-ril-a #x0C09)

(specop-z iilh    zformat-ri-a  #x0A52)

(specop-z iill    zformat-ri-a  #x0A53)

(specop-z ipk     zformat-s     #xB20B)

(specop-z ipm     zformat-rre   #xB222)

(specop-z ipte    zformat-rrf-a #xB221)

(specop-z irbm    zformat-rre   #xB2AC)

(specop-z iske    zformat-rre   #xB229)

(specop-z ivsk    zformat-rre   #xB223)

(specop-z kdb     zformat-rxe   #xED18)

(specop-z kdbr    zformat-rre   #xB318)

(specop-z kdsa    zformat-rre   #xB93A)

(specop-z kdtr    zformat-rre   #xB3E0)

(specop-z keb     zformat-rxe   #xED08)

(specop-z kebr    zformat-rre   #xB308)

(specop-z kimd    zformat-rre   #xB93E)

(specop-z klmd    zformat-rre   #xB93F)

(specop-z km      zformat-rre   #xB92E)

(specop-z kma     zformat-rrf-b #xB929)

(specop-z kmac    zformat-rre   #xB91E)

(specop-z kmc     zformat-rre   #xB92F)

(specop-z kmctr   zformat-rrf-b #xB92D)

(specop-z kmf     zformat-rre   #xB92A)

(specop-z kmo     zformat-rre   #xB92B)

(specop-z kxbr    zformat-rre   #xB348)

(specop-z kxtr    zformat-rre   #xB3E8)

(specop-z l       zformat-rx-a  #x0058)

(specop-z la      zformat-rx-a  #x0041)

(specop-z laa     zformat-rsy-a #xEBF8)

(specop-z laag    zformat-rsy-a #xEBE8)

(specop-z laal    zformat-rsy-a #xEBFA)

(specop-z laalg   zformat-rsy-a #xEBEA)

(specop-z lae     zformat-rx-a  #x0051)

(specop-z laey    zformat-rxy-a #xE375)

(specop-z lam     zformat-rs-a  #x009A)

(specop-z lamy    zformat-rsy-a #xEB9A)

(specop-z lan     zformat-rsy-a #xEBF4)

(specop-z lang    zformat-rsy-a #xEBE4)

(specop-z lao     zformat-rsy-a #xEBF6)

(specop-z laog    zformat-rsy-a #xEBE6)

(specop-z larl    zformat-ril-b #x0C00)

(specop-z lasp    zformat-sse   #xE500)

(specop-z lat     zformat-rxy-a #xE39F)

(specop-z lax     zformat-rsy-a #xEBF7)

(specop-z laxg    zformat-rsy-a #xEBE7)

(specop-z lay     zformat-rxy-a #xE371)

(specop-z lb      zformat-rxy-a #xE376)

(specop-z lbh     zformat-rxy-a #xE3C0)

(specop-z lbr     zformat-rre   #xB926)

(specop-z lcbb    zformat-rxe   #xE727)

(specop-z lcdbr   zformat-rre   #xB313)

(specop-z lcdfr   zformat-rre   #xB373)

(specop-z lcdr    zformat-rr    #x0023)

(specop-z lcebr   zformat-rre   #xB303)

(specop-z lcer    zformat-rr    #x0033)

(specop-z lcgfr   zformat-rre   #xB913)

(specop-z lcgr    zformat-rre   #xB903)

(specop-z lcr     zformat-rr    #x0013)

(specop-z lctl    zformat-rs-a  #x00B7)

(specop-z lctlg   zformat-rsy-a #xEB2F)

(specop-z lcxbr   zformat-rre   #xB343)

(specop-z lcxr    zformat-rre   #xB363)

(specop-z ld      zformat-rx-a  #x0068)

(specop-z lde     zformat-rxe   #xED24)

(specop-z ldeb    zformat-rxe   #xED04)

(specop-z ldebr   zformat-rre   #xB304)

(specop-z lder    zformat-rre   #xB324)

(specop-z ldetr   zformat-rrf-d #xB3D4)

(specop-z ldgr    zformat-rre   #xB3C1)

(specop-z ldr     zformat-rr    #x0028)

(specop-z ldxbr   zformat-rre   #xB345)

(specop-z ldxbra  zformat-rrf-e #xB345)

(specop-z ldxr    zformat-rr    #x0025)

(specop-z ldxtr   zformat-rrf-e #xB3DD)

(specop-z ldy     zformat-rxy-a #xED65)

(specop-z le      zformat-rx-a  #x0078)

(specop-z ledbr   zformat-rre   #xB344)

(specop-z ledbra  zformat-rrf-e #xB344)

(specop-z ledr    zformat-rr    #x0035)

(specop-z ledtr   zformat-rrf-e #xB3D5)

(specop-z ler     zformat-rr    #x0038)

(specop-z lexbr   zformat-rre   #xB346)

(specop-z lexbra  zformat-rrf-e #xB346)

(specop-z lexr    zformat-rre   #xB366)

(specop-z ley     zformat-rxy-a #xED64)

(specop-z lfas    zformat-s     #xB2BD)

(specop-z lfh     zformat-rxy-a #xE3CA)

(specop-z lfhat   zformat-rxy-a #xE3C8)

(specop-z lfpc    zformat-s     #xB29D)

(specop-z lg      zformat-rxy-a #xE304)

(specop-z lgat    zformat-rxy-a #xE385)

(specop-z lgb     zformat-rxy-a #xE377)

(specop-z lgbr    zformat-rre   #xB906)

(specop-z lgdr    zformat-rre   #xB3CD)

(specop-z lgf     zformat-rxy-a #xE314)

(specop-z lgfi    zformat-ril-a #x0C01)

(specop-z lgfr    zformat-rre   #xB914)

(specop-z lgfrl   zformat-ril-b #x0C4C)

(specop-z lgg     zformat-rxy-a #xE34C)

(specop-z lgh     zformat-rxy-a #xE315)

(specop-z lghi    zformat-ri-a  #x0A79)

(specop-z lghr    zformat-rre   #xB907)

(specop-z lghrl   zformat-ril-b #x0C44)

(specop-z lgr     zformat-rre   #xB904)

(specop-z lgrl    zformat-ril-b #x0C48)

(specop-z lgsc    zformat-rxy-a #xE34D)

(specop-z lh      zformat-rx-a  #x0048)

(specop-z lhh     zformat-rxy-a #xE3C4)

(specop-z lhi     zformat-ri-a  #x0A78)

(specop-z lhr     zformat-rre   #xB927)

(specop-z lhrl    zformat-ril-b #x0C45)

(specop-z lhy     zformat-rxy-a #xE378)

(specop-z llc     zformat-rxy-a #xE394)

(specop-z llch    zformat-rxy-a #xE3C2)

(specop-z llcr    zformat-rre   #xB994)

(specop-z llgc    zformat-rxy-a #xE390)

(specop-z llgcr   zformat-rre   #xB984)

(specop-z llgf    zformat-rxy-a #xE316)

(specop-z llgfat  zformat-rxy-a #xE39D)

(specop-z llgfr   zformat-rre   #xB916)

(specop-z llgfrl  zformat-ril-b #x0C4E)

(specop-z llgfsg  zformat-rxy-a #xE348)

(specop-z llgh    zformat-rxy-a #xE391)

(specop-z llghr   zformat-rre   #xB985)

(specop-z llghrl  zformat-ril-b #x0C46)

(specop-z llgt    zformat-rxy-a #xE317)

(specop-z llgtat  zformat-rxy-a #xE39C)

(specop-z llgtr   zformat-rre   #xB917)

(specop-z llh     zformat-rxy-a #xE395)

(specop-z llhh    zformat-rxy-a #xE3C6)

(specop-z llhr    zformat-rre   #xB995)

(specop-z llhrl   zformat-ril-b #x0C42)

(specop-z llihf   zformat-ril-a #x0C0E)

(specop-z llihh   zformat-ri-a  #x0A5C)

(specop-z llihl   zformat-ri-a  #x0A5D)

(specop-z llilf   zformat-ril-a #x0C0F)

(specop-z llilh   zformat-ri-a  #x0A5E)

(specop-z llill   zformat-ri-a  #x0A5F)

(specop-z llzrgf  zformat-rxy-a #xE33A)

(specop-z lm      zformat-rs-a  #x0098)

(specop-z lmd     zformat-ss-e  #x00EF)

(specop-z lmg     zformat-rsy-a #xEB04)

(specop-z lmh     zformat-rsy-a #xEB96)

(specop-z lmy     zformat-rsy-a #xEB98)

(specop-z lndbr   zformat-rre   #xB311)

(specop-z lndfr   zformat-rre   #xB371)

(specop-z lndr    zformat-rr    #x0021)

(specop-z lnebr   zformat-rre   #xB301)

(specop-z lner    zformat-rr    #x0031)

(specop-z lngfr   zformat-rre   #xB911)

(specop-z lngr    zformat-rre   #xB901)

(specop-z lnr     zformat-rr    #x0011)

(specop-z lnxbr   zformat-rre   #xB341)

(specop-z lnxr    zformat-rre   #xB361)

(specop-z loc     zformat-rsy-b #xEBF2)

(specop-z locfh   zformat-rsy-b #xEBE0)

(specop-z locfhr  zformat-rrf-c #xB9E0)

(specop-z locg    zformat-rsy-b #xEBE2)

(specop-z locghi  zformat-rie-g #xEC46)

(specop-z locgr   zformat-rrf-c #xB9E2)

(specop-z lochhi  zformat-rie-g #xEC4E)

(specop-z lochi   zformat-rie-g #xEC42)

(specop-z locr    zformat-rrf-c #xB9F2)

(specop-z lpd     zformat-ssf   #x0C84)

(specop-z lpdbr   zformat-rre   #xB310)

(specop-z lpdfr   zformat-rre   #xB370)

(specop-z lpdg    zformat-ssf   #x0C85)

(specop-z lpdr    zformat-rr    #x0020)

(specop-z lpebr   zformat-rre   #xB300)

(specop-z lper    zformat-rr    #x0030)

(specop-z lpgfr   zformat-rre   #xB910)

(specop-z lpgr    zformat-rre   #xB900)

(specop-z lpq     zformat-rxy-a #xE38F)

(specop-z lpr     zformat-rr    #x0010)

(specop-z lpsw    zformat-si    #x0082)

(specop-z lpswe   zformat-s     #xB2B2)

(specop-z lptea   zformat-rrf-b #xB9AA)

(specop-z lpxbr   zformat-rre   #xB340)

(specop-z lpxr    zformat-rre   #xB360)

(specop-z lr      zformat-rr    #x0018)

(specop-z lra     zformat-rx-a  #x00B1)

(specop-z lrag    zformat-rxy-a #xE303)

(specop-z lray    zformat-rxy-a #xE313)

(specop-z lrdr    zformat-rr    #x0025)

(specop-z lrer    zformat-rr    #x0035)

(specop-z lrl     zformat-ril-b #x0C4D)

(specop-z lrv     zformat-rxy-a #xE31E)

(specop-z lrvg    zformat-rxy-a #xE30F)

(specop-z lrvgr   zformat-rre   #xB90F)

(specop-z lrvh    zformat-rxy-a #xE31F)

(specop-z lrvr    zformat-rre   #xB91F)

(specop-z lt      zformat-rxy-a #xE312)

(specop-z ltdbr   zformat-rre   #xB312)

(specop-z ltdr    zformat-rr    #x0022)

(specop-z ltdtr   zformat-rre   #xB3D6)

(specop-z ltebr   zformat-rre   #xB302)

(specop-z lter    zformat-rr    #x0032)

(specop-z ltg     zformat-rxy-a #xE302)

(specop-z ltgf    zformat-rxy-a #xE332)

(specop-z ltgfr   zformat-rre   #xB912)

(specop-z ltgr    zformat-rre   #xB902)

(specop-z ltr     zformat-rr    #x0012)

(specop-z ltxbr   zformat-rre   #xB342)

(specop-z ltxr    zformat-rre   #xB362)

(specop-z ltxtr   zformat-rre   #xB3DE)

(specop-z lura    zformat-rre   #xB24B)

(specop-z lurag   zformat-rre   #xB905)

(specop-z lxd     zformat-rxe   #xED25)

(specop-z lxdb    zformat-rxe   #xED05)

(specop-z lxdbr   zformat-rre   #xB305)

(specop-z lxdr    zformat-rre   #xB325)

(specop-z lxdtr   zformat-rrf-d #xB3DC)

(specop-z lxe     zformat-rxe   #xED26)

(specop-z lxeb    zformat-rxe   #xED06)

(specop-z lxebr   zformat-rre   #xB306)

(specop-z lxer    zformat-rre   #xB326)

(specop-z lxr     zformat-rre   #xB365)

(specop-z ly      zformat-rxy-a #xE358)

(specop-z lzdr    zformat-rre   #xB375)

(specop-z lzer    zformat-rre   #xB374)

(specop-z lzrf    zformat-rxy-a #xE33B)

(specop-z lzrg    zformat-rxy-a #xE32A)

(specop-z lzxr    zformat-rre   #xB376)

(specop-z m       zformat-rx-a  #x005C)

(specop-z mad     zformat-rxf   #xED3E)

(specop-z madb    zformat-rxf   #xED1E)

(specop-z madbr   zformat-rrd   #xB31E)

(specop-z madr    zformat-rrd   #xB33E)

(specop-z mae     zformat-rxf   #xED2E)

(specop-z maeb    zformat-rxf   #xED0E)

(specop-z maebr   zformat-rrd   #xB30E)

(specop-z maer    zformat-rrd   #xB32E)

(specop-z may     zformat-rxf   #xED3A)

(specop-z mayh    zformat-rxf   #xED3C)

(specop-z mayhr   zformat-rrd   #xB33C)

(specop-z mayl    zformat-rxf   #xED38)

(specop-z maylr   zformat-rrd   #xB338)

(specop-z mayr    zformat-rrd   #xB33A)

(specop-z mc      zformat-si    #x00AF)

(specop-z md      zformat-rx-a  #x006C)

(specop-z mdb     zformat-rxe   #xED1C)

(specop-z mdbr    zformat-rre   #xB31C)

(specop-z mde     zformat-rx-a  #x007C)

(specop-z mdeb    zformat-rxe   #xED0C)

(specop-z mdebr   zformat-rre   #xB30C)

(specop-z mder    zformat-rr    #x003C)

(specop-z mdr     zformat-rr    #x002C)

(specop-z mdtr    zformat-rrf-a #xB3D0)

(specop-z mdtra   zformat-rrf-a #xB3D0)

(specop-z me      zformat-rx-a  #x007C)

(specop-z mee     zformat-rxe   #xED37)

(specop-z meeb    zformat-rxe   #xED17)

(specop-z meebr   zformat-rre   #xB317)

(specop-z meer    zformat-rre   #xB337)

(specop-z mer     zformat-rr    #x003C)

(specop-z mfy     zformat-rxy-a #xE35C)

(specop-z mg      zformat-rxy-a #xE384)

(specop-z mgh     zformat-rxy-a #xE33C)

(specop-z mghi    zformat-ri-a  #x0A7D)

(specop-z mgrk    zformat-rrf-a #xB9EC)

(specop-z mh      zformat-rx-a  #x004C)

(specop-z mhi     zformat-ri-a  #x0A7C)

(specop-z mhy     zformat-rxy-a #xE37C)

(specop-z ml      zformat-rxy-a #xE396)

(specop-z mlg     zformat-rxy-a #xE386)

(specop-z mlgr    zformat-rre   #xB986)

(specop-z mlr     zformat-rre   #xB996)

(specop-z mp      zformat-ss-b  #x00FC)

(specop-z mr      zformat-rr    #x001C)

(specop-z ms      zformat-rx-a  #x0071)

(specop-z msc     zformat-rxy-a #xE353)

(specop-z msch    zformat-s     #xB232)

(specop-z msd     zformat-rxf   #xED3F)

(specop-z msdb    zformat-rxf   #xED1F)

(specop-z msdbr   zformat-rrd   #xB31F)

(specop-z msdr    zformat-rrd   #xB33F)

(specop-z mse     zformat-rxf   #xED2F)

(specop-z mseb    zformat-rxf   #xED0F)

(specop-z msebr   zformat-rrd   #xB30F)

(specop-z mser    zformat-rrd   #xB32F)

(specop-z msfi    zformat-ril-a #x0C21)

(specop-z msg     zformat-rxy-a #xE30C)

(specop-z msgc    zformat-rxy-a #xE383)

(specop-z msgf    zformat-rxy-a #xE31C)

(specop-z msgfi   zformat-ril-a #x0C20)

(specop-z msgfr   zformat-rre   #xB91C)

(specop-z msgr    zformat-rre   #xB90C)

(specop-z msgrkc  zformat-rrf-a #xB9ED)

(specop-z msr     zformat-rre   #xB252)

(specop-z msrkc   zformat-rrf-a #xB9FD)

(specop-z msta    zformat-rre   #xB247)

(specop-z msy     zformat-rxy-a #xE351)

(specop-z mvc     zformat-ss-a  #x00D2)

(specop-z mvcdk   zformat-sse   #xE50F)

(specop-z mvcin   zformat-ss-a  #x00E8)

(specop-z mvck    zformat-ss-d  #x00D9)

(specop-z mvcl    zformat-rr    #x000E)

(specop-z mvcle   zformat-rs-a  #x00A8)

(specop-z mvclu   zformat-rsy-a #xEB8E)

(specop-z mvcos   zformat-ssf   #x0C80)

(specop-z mvcp    zformat-ss-d  #x00DA)

(specop-z mvcrl   zformat-sse   #xE50A)

(specop-z mvcs    zformat-ss-d  #x00DB)

(specop-z mvcsk   zformat-sse   #xE50E)

(specop-z mvghi   zformat-sil   #xE548)

(specop-z mvhhi   zformat-sil   #xE544)

(specop-z mvhi    zformat-sil   #xE54C)

(specop-z mvi     zformat-si    #x0092)

(specop-z mviy    zformat-siy   #xEB52)

(specop-z mvn     zformat-ss-a  #x00D1)

(specop-z mvo     zformat-ss-b  #x00F1)

(specop-z mvpg    zformat-rre   #xB254)

(specop-z mvst    zformat-rre   #xB255)

(specop-z mvz     zformat-ss-a  #x00D3)

(specop-z mxbr    zformat-rre   #xB34C)

(specop-z mxd     zformat-rx-a  #x0067)

(specop-z mxdb    zformat-rxe   #xED07)

(specop-z mxdbr   zformat-rre   #xB307)

(specop-z mxdr    zformat-rr    #x0027)

(specop-z mxr     zformat-rr    #x0026)

(specop-z mxtr    zformat-rrf-a #xB3D8)

(specop-z mxtra   zformat-rrf-a #xB3D8)

(specop-z my      zformat-rxf   #xED3B)

(specop-z myh     zformat-rxf   #xED3D)

(specop-z myhr    zformat-rrd   #xB33D)

(specop-z myl     zformat-rxf   #xED39)

(specop-z mylr    zformat-rrd   #xB339)

(specop-z myr     zformat-rrd   #xB33B)

(specop-z n       zformat-rx-a  #x0054)

(specop-z nc      zformat-ss-a  #x00D4)

(specop-z ncgrk   zformat-rrf-a #xB9E5)

(specop-z ncrk    zformat-rrf-a #xB9F5)

(specop-z ng      zformat-rxy-a #xE380)

(specop-z ngr     zformat-rre   #xB980)

(specop-z ngrk    zformat-rrf-a #xB9E4)

(specop-z ni      zformat-si    #x0094)

(specop-z niai    zformat-ie    #xB2FA)

(specop-z nihf    zformat-ril-a #x0C0A)

(specop-z nihh    zformat-ri-a  #x0A54)

(specop-z nihl    zformat-ri-a  #x0A55)

(specop-z nilf    zformat-ril-a #x0C0B)

(specop-z nilh    zformat-ri-a  #x0A56)

(specop-z nill    zformat-ri-a  #x0A57)

(specop-z niy     zformat-siy   #xEB54)

(specop-z nngrk   zformat-rrf-a #xB964)

(specop-z nnrk    zformat-rrf-a #xB974)

(specop-z nogrk   zformat-rrf-a #xB966)

(specop-z nork    zformat-rrf-a #xB976)

(specop-z nr      zformat-rr    #x0014)

(specop-z nrk     zformat-rrf-a #xB9F4)

(specop-z ntstg   zformat-rxy-a #xE325)

(specop-z nxgrk   zformat-rrf-a #xB967)

(specop-z nxrk    zformat-rrf-a #xB977)

(specop-z ny      zformat-rxy-a #xE354)

(specop-z o       zformat-rx-a  #x0056)

(specop-z oc      zformat-ss-a  #x00D6)

(specop-z ocgrk   zformat-rrf-a #xB965)

(specop-z ocrk    zformat-rrf-a #xB975)

(specop-z og      zformat-rxy-a #xE381)

(specop-z ogr     zformat-rre   #xB981)

(specop-z ogrk    zformat-rrf-a #xB9E6)

(specop-z oi      zformat-si    #x0096)

(specop-z oihf    zformat-ril-a #x0C0C)

(specop-z oihh    zformat-ri-a  #x0A58)

(specop-z oihl    zformat-ri-a  #x0A59)

(specop-z oilf    zformat-ril-a #x0C0D)

(specop-z oilh    zformat-ri-a  #x0A5A)

(specop-z oill    zformat-ri-a  #x0A5B)

(specop-z oiy     zformat-siy   #xEB56)

(specop-z or      zformat-rr    #x0016)

(specop-z ork     zformat-rrf-a #xB9F6)

(specop-z oy      zformat-rxy-a #xE356)

(specop-z pack    zformat-ss-b  #x00F2)

(specop-z palb    zformat-rre   #xB248)

(specop-z pc      zformat-s     #xB218)

(specop-z pcc     zformat-rre   #xB92C)

(specop-z pckmo   zformat-rre   #xB928)

(specop-z pfd     zformat-rxy-b #xE336)

(specop-z pfdrl   zformat-ril-c #x0C62)

(specop-z pfmf    zformat-rre   #xB9AF)

(specop-z pfpo    zformat-e     #x010A)

(specop-z pgin    zformat-rre   #xB22E)

(specop-z pgout   zformat-rre   #xB22F)

(specop-z pka     zformat-ss-f  #x00E9)

(specop-z pku     zformat-ss-f  #x00E1)

(specop-z plo     zformat-ss-e  #x00EE)

(specop-z popcnt  zformat-rrf-c #xB9E1)

(specop-z ppa     zformat-rrf-c #xB2E8)

(specop-z pr      zformat-e     #x0101)

(specop-z prno    zformat-rre   #xB93C)

(specop-z pt      zformat-rre   #xB228)

(specop-z ptf     zformat-rre   #xB9A2)

(specop-z ptff    zformat-e     #x0104)

(specop-z pti     zformat-rre   #xB99E)

(specop-z ptlb    zformat-s     #xB20D)

(specop-z qadtr   zformat-rrf-b #xB3F5)

(specop-z qaxtr   zformat-rrf-b #xB3FD)

(specop-z rchp    zformat-s     #xB23B)

(specop-z risbg   zformat-rie-f #xEC55)

(specop-z risbgn  zformat-rie-f #xEC59)

(specop-z risbhg  zformat-rie-f #xEC5D)

(specop-z risblg  zformat-rie-f #xEC51)

(specop-z rll     zformat-rsy-a #xEB1D)

(specop-z rllg    zformat-rsy-a #xEB1C)

(specop-z rnsbg   zformat-rie-f #xEC54)

(specop-z rosbg   zformat-rie-f #xEC56)

(specop-z rp      zformat-s     #xB277)

(specop-z rrbe    zformat-rre   #xB22A)

(specop-z rrbm    zformat-rre   #xB9AE)

(specop-z rrdtr   zformat-rrf-b #xB3F7)

(specop-z rrxtr   zformat-rrf-b #xB3FF)

(specop-z rsch    zformat-s     #xB238)

(specop-z rxsbg   zformat-rie-f #xEC57)

(specop-z s       zformat-rx-a  #x005B)

(specop-z sac     zformat-s     #xB219)

(specop-z sacf    zformat-s     #xB279)

(specop-z sal     zformat-s     #xB237)

(specop-z sam24   zformat-e     #x010C)

(specop-z sam31   zformat-e     #x010D)

(specop-z sam64   zformat-e     #x010E)

(specop-z sar     zformat-rre   #xB24E)

(specop-z schm    zformat-s     #xB23C)

(specop-z sck     zformat-s     #xB204)

(specop-z sckc    zformat-s     #xB206)

(specop-z sckpf   zformat-e     #x0107)

(specop-z sd      zformat-rx-a  #x006B)

(specop-z sdb     zformat-rxe   #xED1B)

(specop-z sdbr    zformat-rre   #xB31B)

(specop-z sdr     zformat-rr    #x002B)

(specop-z sdtr    zformat-rrf-a #xB3D3)

(specop-z sdtra   zformat-rrf-a #xB3D3)

(specop-z se      zformat-rx-a  #x007B)

(specop-z seb     zformat-rxe   #xED0B)

(specop-z sebr    zformat-rre   #xB30B)

(specop-z selfhr  zformat-rrf-a #xB9C0)

(specop-z selgr   zformat-rrf-a #xB9E3)

(specop-z selr    zformat-rrf-a #xB9F0)

(specop-z ser     zformat-rr    #x003B)

(specop-z sfasr   zformat-rre   #xB385)

(specop-z sfpc    zformat-rre   #xB384)

(specop-z sg      zformat-rxy-a #xE309)

(specop-z sgf     zformat-rxy-a #xE319)

(specop-z sgfr    zformat-rre   #xB919)

(specop-z sgh     zformat-rxy-a #xE339)

(specop-z sgr     zformat-rre   #xB909)

(specop-z sgrk    zformat-rrf-a #xB9E9)

(specop-z sh      zformat-rx-a  #x004B)

(specop-z shhhr   zformat-rrf-a #xB9C9)

(specop-z shhlr   zformat-rrf-a #xB9D9)

(specop-z shy     zformat-rxy-a #xE37B)

(specop-z sie     zformat-s     #xB214)

(specop-z sigp    zformat-rs-a  #x00AE)

(specop-z sl      zformat-rx-a  #x005F)

(specop-z sla     zformat-rs-a  #x008B)

(specop-z slag    zformat-rsy-a #xEB0B)

(specop-z slak    zformat-rsy-a #xEBDD)

(specop-z slb     zformat-rxy-a #xE399)

(specop-z slbg    zformat-rxy-a #xE389)

(specop-z slbgr   zformat-rre   #xB989)

(specop-z slbr    zformat-rre   #xB999)

(specop-z slda    zformat-rs-a  #x008F)

(specop-z sldl    zformat-rs-a  #x008D)

(specop-z sldt    zformat-rxf   #xED40)

(specop-z slfi    zformat-ril-a #x0C25)

(specop-z slg     zformat-rxy-a #xE30B)

(specop-z slgf    zformat-rxy-a #xE31B)

(specop-z slgfi   zformat-ril-a #x0C24)

(specop-z slgfr   zformat-rre   #xB91B)

(specop-z slgr    zformat-rre   #xB90B)

(specop-z slgrk   zformat-rrf-a #xB9EB)

(specop-z slhhhr  zformat-rrf-a #xB9CB)

(specop-z slhhlr  zformat-rrf-a #xB9DB)

(specop-z sll     zformat-rs-a  #x0089)

(specop-z sllg    zformat-rsy-a #xEB0D)

(specop-z sllk    zformat-rsy-a #xEBDF)

(specop-z slr     zformat-rr    #x001F)

(specop-z slrk    zformat-rrf-a #xB9FB)

(specop-z slxt    zformat-rxf   #xED48)

(specop-z sly     zformat-rxy-a #xE35F)

(specop-z sp      zformat-ss-b  #x00FB)

(specop-z spka    zformat-s     #xB20A)

(specop-z spm     zformat-rr    #x0004)

(specop-z spt     zformat-s     #xB208)

(specop-z spx     zformat-s     #xB210)

(specop-z sqd     zformat-rxe   #xED35)

(specop-z sqdb    zformat-rxe   #xED15)

(specop-z sqdbr   zformat-rre   #xB315)

(specop-z sqdr    zformat-rre   #xB244)

(specop-z sqe     zformat-rxe   #xED34)

(specop-z sqeb    zformat-rxe   #xED14)

(specop-z sqebr   zformat-rre   #xB314)

(specop-z sqer    zformat-rre   #xB245)

(specop-z sqxbr   zformat-rre   #xB316)

(specop-z sqxr    zformat-rre   #xB336)

(specop-z sr      zformat-rr    #x001B)

(specop-z sra     zformat-rs-a  #x008A)

(specop-z srag    zformat-rsy-a #xEB0A)

(specop-z srak    zformat-rsy-a #xEBDC)

(specop-z srda    zformat-rs-a  #x008E)

(specop-z srdl    zformat-rs-a  #x008C)

(specop-z srdt    zformat-rxf   #xED41)

(specop-z srk     zformat-rrf-a #xB9F9)

(specop-z srl     zformat-rs-a  #x0088)

(specop-z srlg    zformat-rsy-a #xEB0C)

(specop-z srlk    zformat-rsy-a #xEBDE)

(specop-z srnm    zformat-s     #xB299)

(specop-z srnmb   zformat-s     #xB2B8)

(specop-z srnmt   zformat-s     #xB2B9)

(specop-z srp     zformat-ss-c  #x00F0)

(specop-z srst    zformat-rre   #xB25E)

(specop-z srstu   zformat-rre   #xB9BE)

(specop-z srxt    zformat-rxf   #xED49)

(specop-z ssair   zformat-rre   #xB99F)

(specop-z ssar    zformat-rre   #xB225)

(specop-z ssch    zformat-s     #xB233)

(specop-z sske    zformat-rrf-d #xB22B)

(specop-z ssm     zformat-si    #x0080)

(specop-z st      zformat-rx-a  #x0050)

(specop-z stam    zformat-rs-a  #x009B)

(specop-z stamy   zformat-rsy-a #xEB9B)

(specop-z stap    zformat-s     #xB212)

(specop-z stc     zformat-rx-a  #x0042)

(specop-z stch    zformat-rxy-a #xE3C3)

(specop-z stck    zformat-s     #xB205)

(specop-z stckc   zformat-s     #xB207)

(specop-z stcke   zformat-s     #xB278)

(specop-z stckf   zformat-s     #xB27C)

(specop-z stcm    zformat-rs-b  #x00BE)

(specop-z stcmh   zformat-rsy-b #xEB2C)

(specop-z stcmy   zformat-rsy-b #xEB2D)

(specop-z stcps   zformat-s     #xB23A)

(specop-z stcrw   zformat-s     #xB239)

(specop-z stctg   zformat-rsy-a #xEB25)

(specop-z stctl   zformat-rs-a  #x00B6)

(specop-z stcy    zformat-rxy-a #xE372)

(specop-z std     zformat-rx-a  #x0060)

(specop-z stdy    zformat-rxy-a #xED67)

(specop-z ste     zformat-rx-a  #x0070)

(specop-z stey    zformat-rxy-a #xED66)

(specop-z stfh    zformat-rxy-a #xE3CB)

(specop-z stfl    zformat-s     #xB2B1)

(specop-z stfle   zformat-s     #xB2B0)

(specop-z stfpc   zformat-s     #xB29C)

(specop-z stg     zformat-rxy-a #xE324)

(specop-z stgrl   zformat-ril-b #x0C4B)

(specop-z stgsc   zformat-rxy-a #xE349)

(specop-z sth     zformat-rx-a  #x0040)

(specop-z sthh    zformat-rxy-a #xE3C7)

(specop-z sthrl   zformat-ril-b #x0C47)

(specop-z sthy    zformat-rxy-a #xE370)

(specop-z stidp   zformat-s     #xB202)

(specop-z stm     zformat-rs-a  #x0090)

(specop-z stmg    zformat-rsy-a #xEB24)

(specop-z stmh    zformat-rsy-a #xEB26)

(specop-z stmy    zformat-rsy-a #xEB90)

(specop-z stnsm   zformat-si    #x00AC)

(specop-z stoc    zformat-rsy-b #xEBF3)

(specop-z stocfh  zformat-rsy-b #xEBE1)

(specop-z stocg   zformat-rsy-b #xEBE3)

(specop-z stosm   zformat-si    #x00AD)

(specop-z stpq    zformat-rxy-a #xE38E)

(specop-z stpt    zformat-s     #xB209)

(specop-z stpx    zformat-s     #xB211)

(specop-z strag   zformat-sse   #xE502)

(specop-z strl    zformat-ril-b #x0C4F)

(specop-z strv    zformat-rxy-a #xE33E)

(specop-z strvg   zformat-rxy-a #xE32F)

(specop-z strvh   zformat-rxy-a #xE33F)

(specop-z stsch   zformat-s     #xB234)

(specop-z stsi    zformat-s     #xB27D)

(specop-z stura   zformat-rre   #xB246)

(specop-z sturg   zformat-rre   #xB925)

(specop-z sty     zformat-rxy-a #xE350)

(specop-z su      zformat-rx-a  #x007F)

(specop-z sur     zformat-rr    #x003F)

(specop-z svc     zformat-i     #x000A)

(specop-z sw      zformat-rx-a  #x006F)

(specop-z swr     zformat-rr    #x002F)

(specop-z sxbr    zformat-rre   #xB34B)

(specop-z sxr     zformat-rr    #x0037)

(specop-z sxtr    zformat-rrf-a #xB3DB)

(specop-z sxtra   zformat-rrf-a #xB3DB)

(specop-z sy      zformat-rxy-a #xE35B)

(specop-z tabort  zformat-s     #xB2FC)

(specop-z tam     zformat-e     #x010B)

(specop-z tar     zformat-rre   #xB24C)

(specop-z tb      zformat-rre   #xB22C)

(specop-z tbdr    zformat-rrf-e #xB351)

(specop-z tbedr   zformat-rrf-e #xB350)

(specop-z tbegin  zformat-sil   #xE560)

(specop-z tbeginc zformat-sil   #xE561)

(specop-z tcdb    zformat-rxe   #xED11)

(specop-z tceb    zformat-rxe   #xED10)

(specop-z tcxb    zformat-rxe   #xED12)

(specop-z tdcdt   zformat-rxe   #xED54)

(specop-z tdcet   zformat-rxe   #xED50)

(specop-z tdcxt   zformat-rxe   #xED58)

(specop-z tdgdt   zformat-rxe   #xED55)

(specop-z tdget   zformat-rxe   #xED51)

(specop-z tdgxt   zformat-rxe   #xED59)

(specop-z tend    zformat-s     #xB2F8)

(specop-z thder   zformat-rre   #xB358)

(specop-z thdr    zformat-rre   #xB359)

(specop-z tm      zformat-si    #x0091)

(specop-z tmh     zformat-ri-a  #x0A70)

(specop-z tmhh    zformat-ri-a  #x0A72)

(specop-z tmhl    zformat-ri-a  #x0A73)

(specop-z tml     zformat-ri-a  #x0A71)

(specop-z tmlh    zformat-ri-a  #x0A70)

(specop-z tmll    zformat-ri-a  #x0A71)

(specop-z tmy     zformat-siy   #xEB51)

(specop-z tp      zformat-rsl-a #xEBC0)

(specop-z tpei    zformat-rre   #xB9A1)

(specop-z tpi     zformat-s     #xB236)

(specop-z tprot   zformat-sse   #xE501)

(specop-z tr      zformat-ss-a  #x00DC)

(specop-z trace   zformat-rs-a  #x0099)

(specop-z tracg   zformat-rsy-a #xEB0F)

(specop-z trap2   zformat-e     #x01FF)

(specop-z trap4   zformat-s     #xB2FF)

(specop-z tre     zformat-rre   #xB2A5)

(specop-z troo    zformat-rrf-c #xB993)

(specop-z trot    zformat-rrf-c #xB992)

(specop-z trt     zformat-ss-a  #x00DD)

(specop-z trte    zformat-rrf-c #xB9BF)

(specop-z trto    zformat-rrf-c #xB991)

(specop-z trtr    zformat-ss-a  #x00D0)

(specop-z trtre   zformat-rrf-a #xB9BD)

(specop-z trtt    zformat-rrf-c #xB990)

(specop-z ts      zformat-si    #x0093)

(specop-z tsch    zformat-s     #xB235)

(specop-z unpk    zformat-ss-b  #x00F3)

(specop-z unpka   zformat-ss-a  #x00EA)

(specop-z unpku   zformat-ss-a  #x00E2)

(specop-z upt     zformat-e     #x0102)

(specop-z va      zformat-vrr-c #xE7F3)

(specop-z vac     zformat-vrr-d #xE7BB)

(specop-z vacc    zformat-vrr-c #xE7F1)

(specop-z vaccc   zformat-vrr-d #xE7B9)

(specop-z vap     zformat-vri-f #xE671)

(specop-z vavg    zformat-vrr-c #xE7F2)

(specop-z vavgl   zformat-vrr-c #xE7F0)

(specop-z vbperm  zformat-vrr-c #xE785)

(specop-z vcdg    zformat-vrr-a #xE7C3)

(specop-z vcdlg   zformat-vrr-a #xE7C1)

(specop-z vceq    zformat-vrr-b #xE7F8)

(specop-z vcfpl   zformat-vrr-a #xE7C1)

(specop-z vcfps   zformat-vrr-a #xE7C3)

(specop-z vcgd    zformat-vrr-a #xE7C2)

(specop-z vch     zformat-vrr-b #xE7FB)

(specop-z vchl    zformat-vrr-b #xE7F9)

(specop-z vcksm   zformat-vrr-c #xE766)

(specop-z vclfp   zformat-vrr-a #xE7C0)

(specop-z vclgd   zformat-vrr-a #xE7C0)

(specop-z vclz    zformat-vrr-a #xE753)

(specop-z vcp     zformat-vrr-h #xE677)

(specop-z vcsfp   zformat-vrr-a #xE7C2)

(specop-z vctz    zformat-vrr-a #xE752)

(specop-z vcvb    zformat-vrr-i #xE650)

(specop-z vcvbg   zformat-vrr-i #xE652)

(specop-z vcvd    zformat-vri-i #xE658)

(specop-z vcvdg   zformat-vri-i #xE65A)

(specop-z vdp     zformat-vri-f #xE67A)

(specop-z vec     zformat-vrr-a #xE7DB)

(specop-z vecl    zformat-vrr-a #xE7D9)

(specop-z verim   zformat-vri-d #xE772)

(specop-z verll   zformat-vrs-a #xE733)

(specop-z verllv  zformat-vrr-c #xE773)

(specop-z vesl    zformat-vrs-a #xE730)

(specop-z veslv   zformat-vrr-c #xE770)

(specop-z vesra   zformat-vrs-a #xE73A)

(specop-z vesrav  zformat-vrr-c #xE77A)

(specop-z vesrl   zformat-vrs-a #xE738)

(specop-z vesrlv  zformat-vrr-c #xE778)

(specop-z vfa     zformat-vrr-c #xE7E3)

(specop-z vfae    zformat-vrr-b #xE782)

(specop-z vfce    zformat-vrr-c #xE7E8)

(specop-z vfch    zformat-vrr-c #xE7EB)

(specop-z vfche   zformat-vrr-c #xE7EA)

(specop-z vfd     zformat-vrr-c #xE7E5)

(specop-z vfee    zformat-vrr-b #xE780)

(specop-z vfene   zformat-vrr-b #xE781)

(specop-z vfi     zformat-vrr-a #xE7C7)

(specop-z vfll    zformat-vrr-a #xE7C4)

(specop-z vflr    zformat-vrr-a #xE7C5)

(specop-z vfm     zformat-vrr-c #xE7E7)

(specop-z vfma    zformat-vrr-e #xE78F)

(specop-z vfmax   zformat-vrr-c #xE7EF)

(specop-z vfmin   zformat-vrr-c #xE7EE)

(specop-z vfms    zformat-vrr-e #xE78E)

(specop-z vfnma   zformat-vrr-e #xE79F)

(specop-z vfnms   zformat-vrr-e #xE79E)

(specop-z vfpso   zformat-vrr-a #xE7CC)

(specop-z vfs     zformat-vrr-c #xE7E2)

(specop-z vfsq    zformat-vrr-a #xE7CE)

(specop-z vftci   zformat-vri-e #xE74A)

(specop-z vgbm    zformat-vri-a #xE744)

(specop-z vgef    zformat-vrv   #xE713)

(specop-z vgeg    zformat-vrv   #xE712)

(specop-z vgfm    zformat-vrr-c #xE7B4)

(specop-z vgfma   zformat-vrr-d #xE7BC)

(specop-z vgm     zformat-vri-b #xE746)

(specop-z vistr   zformat-vrr-a #xE75C)

(specop-z vl      zformat-vrx   #xE706)

(specop-z vlbb    zformat-vrx   #xE707)

(specop-z vlbr    zformat-vrx   #xE606)

(specop-z vlbrrep zformat-vrx   #xE605)

(specop-z vlc     zformat-vrr-a #xE7DE)

(specop-z vleb    zformat-vrx   #xE700)

(specop-z vlef    zformat-vrx   #xE703)

(specop-z vleg    zformat-vrx   #xE702)

(specop-z vleh    zformat-vrx   #xE701)

(specop-z vleib   zformat-vri-a #xE740)

(specop-z vleif   zformat-vri-a #xE743)

(specop-z vleig   zformat-vri-a #xE742)

(specop-z vleih   zformat-vri-a #xE741)

(specop-z vler    zformat-vrx   #xE607)

(specop-z vlgv    zformat-vrs-c #xE721)

(specop-z vlip    zformat-vri-h #xE649)

(specop-z vll     zformat-vrs-b #xE737)

(specop-z vllebrf zformat-vrx   #xE603)

(specop-z vllebrg zformat-vrx   #xE602)

(specop-z vllebrh zformat-vrx   #xE601)

(specop-z vllebrz zformat-vrx   #xE604)

(specop-z vllez   zformat-vrx   #xE704)

(specop-z vlm     zformat-vrs-a #xE736)

(specop-z vlp     zformat-vrr-a #xE7DF)

(specop-z vlr     zformat-vrr-a #xE756)

(specop-z vlrep   zformat-vrx   #xE705)

(specop-z vlrl    zformat-vsi   #xE635)

(specop-z vlrlr   zformat-vrs-d #xE637)

(specop-z vlvg    zformat-vrs-b #xE722)

(specop-z vlvgp   zformat-vrr-f #xE762)

(specop-z vmae    zformat-vrr-d #xE7AE)

(specop-z vmah    zformat-vrr-d #xE7AB)

(specop-z vmal    zformat-vrr-d #xE7AA)

(specop-z vmale   zformat-vrr-d #xE7AC)

(specop-z vmalh   zformat-vrr-d #xE7A9)

(specop-z vmalo   zformat-vrr-d #xE7AD)

(specop-z vmao    zformat-vrr-d #xE7AF)

(specop-z vme     zformat-vrr-c #xE7A6)

(specop-z vmh     zformat-vrr-c #xE7A3)

(specop-z vml     zformat-vrr-c #xE7A2)

(specop-z vmle    zformat-vrr-c #xE7A4)

(specop-z vmlh    zformat-vrr-c #xE7A1)

(specop-z vmlo    zformat-vrr-c #xE7A5)

(specop-z vmn     zformat-vrr-c #xE7FE)

(specop-z vmnl    zformat-vrr-c #xE7FC)

(specop-z vmo     zformat-vrr-c #xE7A7)

(specop-z vmp     zformat-vri-f #xE678)

(specop-z vmrh    zformat-vrr-c #xE761)

(specop-z vmrl    zformat-vrr-c #xE760)

(specop-z vmsl    zformat-vrr-d #xE6B8)

(specop-z vmsp    zformat-vri-f #xE679)

(specop-z vmx     zformat-vrr-c #xE7FF)

(specop-z vmxl    zformat-vrr-c #xE7FD)

(specop-z vn      zformat-vrr-c #xE768)

(specop-z vnc     zformat-vrr-c #xE769)

(specop-z vnn     zformat-vrr-c #xE76E)

(specop-z vno     zformat-vrr-c #xE76B)

(specop-z vnx     zformat-vrr-c #xE76C)

(specop-z vo      zformat-vrr-c #xE76A)

(specop-z voc     zformat-vrr-c #xE76F)

(specop-z vpdi    zformat-vrr-c #xE784)

(specop-z vperm   zformat-vrr-e #xE78C)

(specop-z vpk     zformat-vrr-c #xE794)

(specop-z vpkls   zformat-vrr-b #xE795)

(specop-z vpks    zformat-vrr-b #xE797)

(specop-z vpkz    zformat-vsi   #xE634)

(specop-z vpopct  zformat-vrr-a #xE750)

(specop-z vpsop   zformat-vri-g #xE65B)

(specop-z vrep    zformat-vri-c #xE74D)

(specop-z vrepi   zformat-vri-a #xE745)

(specop-z vrp     zformat-vri-f #xE67B)

(specop-z vs      zformat-vrr-c #xE7F7)

(specop-z vsbcbi  zformat-vrr-d #xE7BD)

(specop-z vsbi    zformat-vrr-d #xE7BF)

(specop-z vscbi   zformat-vrr-c #xE7F5)

(specop-z vscef   zformat-vrv   #xE71B)

(specop-z vsceg   zformat-vrv   #xE71A)

(specop-z vsdp    zformat-vri-f #xE67E)

(specop-z vseg    zformat-vrr-a #xE75F)

(specop-z vsel    zformat-vrr-e #xE78D)

(specop-z vsl     zformat-vrr-c #xE774)

(specop-z vslb    zformat-vrr-c #xE775)

(specop-z vsld    zformat-vri-d #xE786)

(specop-z vsldb   zformat-vri-d #xE777)

(specop-z vsp     zformat-vri-f #xE673)

(specop-z vsra    zformat-vrr-c #xE77E)

(specop-z vsrab   zformat-vrr-c #xE77F)

(specop-z vsrd    zformat-vri-d #xE787)

(specop-z vsrl    zformat-vrr-c #xE77C)

(specop-z vsrlb   zformat-vrr-c #xE77D)

(specop-z vsrp    zformat-vri-g #xE659)

(specop-z vst     zformat-vrx   #xE70E)

(specop-z vstbr   zformat-vrx   #xE60E)

(specop-z vsteb   zformat-vrx   #xE708)

(specop-z vstebrf zformat-vrx   #xE60B)

(specop-z vstebrg zformat-vrx   #xE60A)

(specop-z vstebrh zformat-vrx   #xE609)

(specop-z vstef   zformat-vrx   #xE70B)

(specop-z vsteg   zformat-vrx   #xE70A)

(specop-z vsteh   zformat-vrx   #xE709)

(specop-z vster   zformat-vrx   #xE60F)

(specop-z vstl    zformat-vrs-b #xE73F)

(specop-z vstm    zformat-vrs-a #xE73E)

(specop-z vstrc   zformat-vrr-d #xE78A)

(specop-z vstrl   zformat-vsi   #xE63D)

(specop-z vstrlr  zformat-vrs-d #xE63F)

(specop-z vstrs   zformat-vrr-d #xE78B)

(specop-z vsum    zformat-vrr-c #xE764)

(specop-z vsumg   zformat-vrr-c #xE765)

(specop-z vsumq   zformat-vrr-c #xE767)

(specop-z vtm     zformat-vrr-a #xE7D8)

(specop-z vtp     zformat-vrr-g #xE65F)

(specop-z vuph    zformat-vrr-a #xE7D7)

(specop-z vupkz   zformat-vsi   #xE63C)

(specop-z vupl    zformat-vrr-a #xE7D6)

(specop-z vuplh   zformat-vrr-a #xE7D5)

(specop-z vupll   zformat-vrr-a #xE7D4)

(specop-z vx      zformat-vrr-c #xE76D)

(specop-z wfc     zformat-vrr-a #xE7CB)

(specop-z wfk     zformat-vrr-a #xE7CA)

(specop-z x       zformat-rx-a  #x0057)

(specop-z xc      zformat-ss-a  #x00D7)

(specop-z xg      zformat-rxy-a #xE382)

(specop-z xgr     zformat-rre   #xB982)

(specop-z xgrk    zformat-rrf-a #xB9E7)

(specop-z xi      zformat-si    #x0097)

(specop-z xihf    zformat-ril-a #x0C06)

(specop-z xilf    zformat-ril-a #x0C07)

(specop-z xiy     zformat-siy   #xEB57)

(specop-z xr      zformat-rr    #x0017)

(specop-z xrk     zformat-rrf-a #xB9F7)

(specop-z xsch    zformat-s     #xB276)

(specop-z xy      zformat-rxy-a #xE357)

(specop-z zap     zformat-ss-b  #x00F8)
