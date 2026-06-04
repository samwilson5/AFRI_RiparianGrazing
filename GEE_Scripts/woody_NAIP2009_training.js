var table = ee.FeatureCollection("projects/uiriparian/assets/Salmon_FO_extent"),
    water = /* color: #1315d6 */ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Point([-113.96305859676954, 45.37802511361812]),
            {
              "class": 2,
              "system:index": "0"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96310151211378, 45.37693988833962]),
            {
              "class": 2,
              "system:index": "1"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96331608883497, 45.375734058047136]),
            {
              "class": 2,
              "system:index": "2"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96355212322828, 45.374241807461566]),
            {
              "class": 2,
              "system:index": "3"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96426022640821, 45.37238774430484]),
            {
              "class": 2,
              "system:index": "4"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96501124493238, 45.370729587556085]),
            {
              "class": 2,
              "system:index": "5"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96458209149, 45.36866436071901]),
            {
              "class": 2,
              "system:index": "6"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96561205975172, 45.366629209414036]),
            {
              "class": 2,
              "system:index": "7"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9661485015547, 45.365875431089584]),
            {
              "class": 2,
              "system:index": "8"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9667064010298, 45.36457890887608]),
            {
              "class": 2,
              "system:index": "9"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96719992748854, 45.36269437593118]),
            {
              "class": 2,
              "system:index": "10"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96552622906324, 45.36136762708402]),
            {
              "class": 2,
              "system:index": "11"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96361649624464, 45.36067408689501]),
            {
              "class": 2,
              "system:index": "12"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9600974380171, 45.360101155978775]),
            {
              "class": 2,
              "system:index": "13"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95737231365797, 45.36234761503116]),
            {
              "class": 2,
              "system:index": "14"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95350993267652, 45.365453310840586]),
            {
              "class": 2,
              "system:index": "15"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94951880566236, 45.36558899269271]),
            {
              "class": 2,
              "system:index": "16"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94642890087721, 45.36501611154777]),
            {
              "class": 2,
              "system:index": "17"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94599974743483, 45.36400601750207]),
            {
              "class": 2,
              "system:index": "18"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9436179458296, 45.362483304332116]),
            {
              "class": 2,
              "system:index": "19"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94441187969801, 45.361156550535156]),
            {
              "class": 2,
              "system:index": "20"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94535601727125, 45.360402699289466]),
            {
              "class": 2,
              "system:index": "21"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94758761517164, 45.35976945648085]),
            {
              "class": 2,
              "system:index": "22"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94840300671217, 45.35791491891453]),
            {
              "class": 2,
              "system:index": "23"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94795239559767, 45.355909537538814]),
            {
              "class": 2,
              "system:index": "24"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94638598553297, 45.35422074020466]),
            {
              "class": 2,
              "system:index": "25"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94514144055006, 45.353059662802494]),
            {
              "class": 2,
              "system:index": "26"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94140780560133, 45.350224724388525]),
            {
              "class": 2,
              "system:index": "27"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93898308865187, 45.34936516713478]),
            {
              "class": 2,
              "system:index": "28"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93668711773512, 45.34981756731658]),
            {
              "class": 2,
              "system:index": "29"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93679440609571, 45.351431098516976]),
            {
              "class": 2,
              "system:index": "30"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9372235595381, 45.35406995227758]),
            {
              "class": 2,
              "system:index": "31"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93645108334181, 45.354823887895215]),
            {
              "class": 2,
              "system:index": "32"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93387616268751, 45.35491435949435]),
            {
              "class": 2,
              "system:index": "33"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9306145965254, 45.353919163948746]),
            {
              "class": 2,
              "system:index": "34"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92771781078932, 45.35245649630899]),
            {
              "class": 2,
              "system:index": "35"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92417729488966, 45.352215227911636]),
            {
              "class": 2,
              "system:index": "36"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92063677899, 45.35103902974566]),
            {
              "class": 2,
              "system:index": "37"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91947806469557, 45.34853654417854]),
            {
              "class": 2,
              "system:index": "38"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92211735836622, 45.3469832532962]),
            {
              "class": 2,
              "system:index": "39"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92486394039747, 45.34467585903048]),
            {
              "class": 2,
              "system:index": "40"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92793238751051, 45.34316770737888]),
            {
              "class": 2,
              "system:index": "41"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93070042721388, 45.34040768581912]),
            {
              "class": 2,
              "system:index": "42"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93102229229567, 45.33762284360678]),
            {
              "class": 2,
              "system:index": "43"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92822699700157, 45.334816080997186]),
            {
              "class": 2,
              "system:index": "44"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92442898903649, 45.33449931732723]),
            {
              "class": 2,
              "system:index": "45"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91831355248253, 45.33050346420254]),
            {
              "class": 2,
              "system:index": "46"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91237543767672, 45.32827664088613]),
            {
              "class": 2,
              "system:index": "47"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91344832128267, 45.32641352829186]),
            {
              "class": 2,
              "system:index": "48"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91337589641805, 45.32403905794724]),
            {
              "class": 2,
              "system:index": "49"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90998558422322, 45.321775990888504]),
            {
              "class": 2,
              "system:index": "50"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90946780123258, 45.32041613926722]),
            {
              "class": 2,
              "system:index": "51"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90955765963254, 45.31804259669755]),
            {
              "class": 2,
              "system:index": "52"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91015191810429, 45.314294679322394]),
            {
              "class": 2,
              "system:index": "53"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90874242459951, 45.312763083795645]),
            {
              "class": 2,
              "system:index": "54"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90709659826807, 45.311149927434045]),
            {
              "class": 2,
              "system:index": "55"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90643959615785, 45.309698059735304]),
            {
              "class": 2,
              "system:index": "56"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90645032499391, 45.30650400238226]),
            {
              "class": 2,
              "system:index": "57"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90613834887597, 45.30482718434008]),
            {
              "class": 2,
              "system:index": "58"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90517275363061, 45.302080386927464]),
            {
              "class": 2,
              "system:index": "59"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89704433844271, 45.293962783706306]),
            {
              "class": 2,
              "system:index": "60"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89483419821444, 45.292012924891836]),
            {
              "class": 2,
              "system:index": "61"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89676538870516, 45.287378466508436]),
            {
              "class": 2,
              "system:index": "62"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89541355536166, 45.284844180618585]),
            {
              "class": 2,
              "system:index": "63"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89522043631258, 45.28411950814148]),
            {
              "class": 2,
              "system:index": "64"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89457670614901, 45.285432970189994]),
            {
              "class": 2,
              "system:index": "65"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89225927756014, 45.287063432494236]),
            {
              "class": 2,
              "system:index": "66"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89137951300326, 45.28655014386028]),
            {
              "class": 2,
              "system:index": "67"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89957634375277, 45.28357599770871]),
            {
              "class": 2,
              "system:index": "68"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9032026903409, 45.278548279356045]),
            {
              "class": 2,
              "system:index": "69"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9032026903409, 45.277808427340865]),
            {
              "class": 2,
              "system:index": "70"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90288082525912, 45.275528414543224]),
            {
              "class": 2,
              "system:index": "71"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90189377234164, 45.27384151481976]),
            {
              "class": 2,
              "system:index": "72"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90109983847323, 45.271848256755526]),
            {
              "class": 2,
              "system:index": "73"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90032736227694, 45.26894884732742]),
            {
              "class": 2,
              "system:index": "74"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90148607657137, 45.26499212258786]),
            {
              "class": 2,
              "system:index": "75"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90399662420931, 45.26299855373504]),
            {
              "class": 2,
              "system:index": "76"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89878240988436, 45.25903824748068]),
            {
              "class": 2,
              "system:index": "77"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9048763887662, 45.25906845569872]),
            {
              "class": 2,
              "system:index": "78"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90468326971713, 45.25728614333599]),
            {
              "class": 2,
              "system:index": "79"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89410463736239, 45.256742375884414]),
            {
              "class": 2,
              "system:index": "80"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90682903692904, 45.254582359306944]),
            {
              "class": 2,
              "system:index": "81"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90556303427401, 45.25332860570777]),
            {
              "class": 2,
              "system:index": "82"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90122858450594, 45.251304414975266]),
            {
              "class": 2,
              "system:index": "83"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90037027762118, 45.250518888693435]),
            {
              "class": 2,
              "system:index": "84"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90013424322787, 45.24564225277998]),
            {
              "class": 2,
              "system:index": "85"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89492002890292, 45.239190606272004]),
            {
              "class": 2,
              "system:index": "86"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89314168634179, 45.236337759478936]),
            {
              "class": 2,
              "system:index": "87"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89526685759657, 45.23001267928437]),
            {
              "class": 2,
              "system:index": "88"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8883574871742, 45.22960465834988]),
            {
              "class": 2,
              "system:index": "89"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88728460356825, 45.22789698339806]),
            {
              "class": 2,
              "system:index": "90"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88895830199354, 45.22864504201456]),
            {
              "class": 2,
              "system:index": "91"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88642629668348, 45.22440591301667]),
            {
              "class": 2,
              "system:index": "92"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88543872452227, 45.22141229911364]),
            {
              "class": 2,
              "system:index": "93"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88824967956988, 45.215426793073284]),
            {
              "class": 2,
              "system:index": "94"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88752011871783, 45.21497331997154]),
            {
              "class": 2,
              "system:index": "95"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8852456054732, 45.21591805151949]),
            {
              "class": 2,
              "system:index": "96"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88358263588397, 45.2144593794181]),
            {
              "class": 2,
              "system:index": "97"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88320712662188, 45.21219031273676]),
            {
              "class": 2,
              "system:index": "98"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88214497185199, 45.20979428591353]),
            {
              "class": 2,
              "system:index": "99"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88148272071295, 45.20778231003093]),
            {
              "class": 2,
              "system:index": "100"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88018745812599, 45.20185915721452]),
            {
              "class": 2,
              "system:index": "101"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88025183114235, 45.198804973452766]),
            {
              "class": 2,
              "system:index": "102"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8842429581565, 45.195925448165355]),
            {
              "class": 2,
              "system:index": "103"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88595957192604, 45.19287094589209]),
            {
              "class": 2,
              "system:index": "104"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88750452431862, 45.19170656275802]),
            {
              "class": 2,
              "system:index": "105"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88791222008888, 45.191010945882624]),
            {
              "class": 2,
              "system:index": "106"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88874906930153, 45.19122265626645]),
            {
              "class": 2,
              "system:index": "107"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88947863015358, 45.19140412168295]),
            {
              "class": 2,
              "system:index": "108"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88926405343238, 45.19037581000603]),
            {
              "class": 2,
              "system:index": "109"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88915676507179, 45.189846524694694]),
            {
              "class": 2,
              "system:index": "110"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88810533913795, 45.19017921889351]),
            {
              "class": 2,
              "system:index": "111"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88606686028663, 45.19131338904702]),
            {
              "class": 2,
              "system:index": "112"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8872684899253, 45.18921137582409]),
            {
              "class": 2,
              "system:index": "113"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89183200048426, 45.187336133043964]),
            {
              "class": 2,
              "system:index": "114"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89485753225306, 45.18422067379113]),
            {
              "class": 2,
              "system:index": "115"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89762557195643, 45.178488388290454]),
            {
              "class": 2,
              "system:index": "116"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9082471196554, 45.17260425496642]),
            {
              "class": 2,
              "system:index": "117"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91090787099817, 45.16842902557729]),
            {
              "class": 2,
              "system:index": "118"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90863335775354, 45.16471493208504]),
            {
              "class": 2,
              "system:index": "119"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90911615537622, 45.1643669597995]),
            {
              "class": 2,
              "system:index": "120"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91364372419336, 45.163701267765525]),
            {
              "class": 2,
              "system:index": "121"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90984966182853, 45.135598985360666]),
            {
              "class": 2,
              "system:index": "122"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90817596340324, 45.13750620465041]),
            {
              "class": 2,
              "system:index": "123"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89834316001206, 45.10708324058447]),
            {
              "class": 2,
              "system:index": "124"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89743186900095, 45.10656388147751]),
            {
              "class": 2,
              "system:index": "125"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88186076285928, 45.09414122641341]),
            {
              "class": 2,
              "system:index": "126"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88171055915444, 45.09380039731228]),
            {
              "class": 2,
              "system:index": "127"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89877567526437, 45.08473934821736]),
            {
              "class": 2,
              "system:index": "128"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89899025198557, 45.083133382342844]),
            {
              "class": 2,
              "system:index": "129"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87448559390897, 45.075838158148166]),
            {
              "class": 2,
              "system:index": "130"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87503276454801, 45.07536084393168]),
            {
              "class": 2,
              "system:index": "131"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87548337566251, 45.075216891242604]),
            {
              "class": 2,
              "system:index": "132"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90912221165479, 45.07187475286416]),
            {
              "class": 2,
              "system:index": "133"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9113752672273, 45.071465598261966]),
            {
              "class": 2,
              "system:index": "134"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91562501120634, 45.07171484310579]),
            {
              "class": 2,
              "system:index": "135"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91683736968108, 45.070063048244705]),
            {
              "class": 2,
              "system:index": "136"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91469160246916, 45.06605126645021]),
            {
              "class": 2,
              "system:index": "137"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91873747288138, 45.063210007865294]),
            {
              "class": 2,
              "system:index": "138"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97177249346713, 45.01615705452127]),
            {
              "class": 2,
              "system:index": "139"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96919757281283, 45.013123256919435]),
            {
              "class": 2,
              "system:index": "140"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97623568926791, 45.0157323325324]),
            {
              "class": 2,
              "system:index": "141"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98447543536166, 45.015307607393424]),
            {
              "class": 2,
              "system:index": "142"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97898227129916, 45.017370528560136]),
            {
              "class": 2,
              "system:index": "143"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97966891680697, 45.015368282606154]),
            {
              "class": 2,
              "system:index": "144"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89348597095511, 44.814529479560605]),
            {
              "class": 2,
              "system:index": "145"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89408678577445, 44.812109146428675]),
            {
              "class": 2,
              "system:index": "146"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90088886783622, 44.815549337944795]),
            {
              "class": 2,
              "system:index": "147"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.9503071368843, 44.47015223802324]),
            {
              "class": 2,
              "system:index": "148"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.94919133793411, 44.46955504641109]),
            {
              "class": 2,
              "system:index": "149"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.95185208927688, 44.46860565229557]),
            {
              "class": 2,
              "system:index": "150"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.95727015148697, 44.46663790725648]),
            {
              "class": 2,
              "system:index": "151"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.96333194386062, 44.46739591806417]),
            {
              "class": 2,
              "system:index": "152"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.95363307606277, 44.4687740943139]),
            {
              "class": 2,
              "system:index": "153"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03829643239486, 44.60236549567422]),
            {
              "class": 2,
              "system:index": "154"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0364296149205, 44.601784936396726]),
            {
              "class": 2,
              "system:index": "155"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03715917577254, 44.6010057556168]),
            {
              "class": 2,
              "system:index": "156"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03597900380599, 44.60086825204148]),
            {
              "class": 2,
              "system:index": "157"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.62715166852354, 45.216108980276175]),
            {
              "class": 2,
              "system:index": "158"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.6280099754083, 45.215504355923606]),
            {
              "class": 2,
              "system:index": "159"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.66910038191432, 45.22881130344524]),
            {
              "class": 2,
              "system:index": "160"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.66300640303248, 45.2384520384598]),
            {
              "class": 2,
              "system:index": "161"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.66468010145778, 45.23881466741996]),
            {
              "class": 2,
              "system:index": "162"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65437295943548, 45.24702313282581]),
            {
              "class": 2,
              "system:index": "163"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.64488866835882, 45.24977259649423]),
            {
              "class": 2,
              "system:index": "164"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.64338663131048, 45.250376856384214]),
            {
              "class": 2,
              "system:index": "165"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.64059713393499, 45.25149472023326]),
            {
              "class": 2,
              "system:index": "166"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65454462081243, 45.25131344650915]),
            {
              "class": 2,
              "system:index": "167"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65617540389349, 45.25167599337882]),
            {
              "class": 2,
              "system:index": "168"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.66862085372259, 45.25225002118939]),
            {
              "class": 2,
              "system:index": "169"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89720467724061, 45.3778413456741]),
            {
              "class": 2,
              "system:index": "170"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89789132274842, 45.377705693212434]),
            {
              "class": 2,
              "system:index": "171"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90151766933656, 45.377283661250544]),
            {
              "class": 2,
              "system:index": "172"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.68570802035264, 45.26756275694915]),
            {
              "class": 2,
              "system:index": "173"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.68553635897568, 45.26617337018182]),
            {
              "class": 2,
              "system:index": "174"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.6619213665288, 44.83843106464255]),
            {
              "class": 2,
              "system:index": "175"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.6604622448247, 44.839846097247936]),
            {
              "class": 2,
              "system:index": "176"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.66252218134814, 44.83824847725892]),
            {
              "class": 2,
              "system:index": "177"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.73117555662765, 44.821897267539946]),
            {
              "class": 2,
              "system:index": "178"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7257467655815, 44.81892929960823]),
            {
              "class": 2,
              "system:index": "179"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.72467388197555, 44.81914238957612]),
            {
              "class": 2,
              "system:index": "180"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99373141625017, 44.74805682198859]),
            {
              "class": 2,
              "system:index": "181"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99080974959132, 44.75192507838278]),
            {
              "class": 2,
              "system:index": "182"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99364216231105, 44.75242794396799]),
            {
              "class": 2,
              "system:index": "183"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99832346272699, 44.75492947388332]),
            {
              "class": 2,
              "system:index": "184"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99877407384149, 44.757047464968636]),
            {
              "class": 2,
              "system:index": "185"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99482777163067, 44.76050075229577]),
            {
              "class": 2,
              "system:index": "186"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9950852636961, 44.763014663353175]),
            {
              "class": 2,
              "system:index": "187"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99750998064556, 44.76901713577983]),
            {
              "class": 2,
              "system:index": "188"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99783184572735, 44.772094303640024]),
            {
              "class": 2,
              "system:index": "189"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99128725573101, 44.77485142864446]),
            {
              "class": 2,
              "system:index": "190"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00186588808575, 44.77842254791903]),
            {
              "class": 2,
              "system:index": "191"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99849703356304, 44.78305271674066]),
            {
              "class": 2,
              "system:index": "192"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00244873925928, 44.79079162350328]),
            {
              "class": 2,
              "system:index": "193"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00268477365259, 44.79547612017318]),
            {
              "class": 2,
              "system:index": "194"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00566739007715, 44.79996788968479]),
            {
              "class": 2,
              "system:index": "195"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.01416586615063, 44.80702351724286]),
            {
              "class": 2,
              "system:index": "196"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.01002453543164, 44.81073802500514]),
            {
              "class": 2,
              "system:index": "197"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00689171530225, 44.811620946579204]),
            {
              "class": 2,
              "system:index": "198"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99886654592969, 44.81481761852402]),
            {
              "class": 2,
              "system:index": "199"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9929442284248, 44.819018689319094]),
            {
              "class": 2,
              "system:index": "200"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99003332580966, 44.82320643725351]),
            {
              "class": 2,
              "system:index": "201"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98945396866245, 44.826929809864154]),
            {
              "class": 2,
              "system:index": "202"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98595014777437, 44.830623851073135]),
            {
              "class": 2,
              "system:index": "203"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98034969535128, 44.831612990010264]),
            {
              "class": 2,
              "system:index": "204"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97897640433565, 44.836225171928795]),
            {
              "class": 2,
              "system:index": "205"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94058047088488, 45.172388390318226]),
            {
              "class": 2,
              "system:index": "206"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94120274337634, 45.17205559214325]),
            {
              "class": 2,
              "system:index": "207"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9355057314287, 45.17196482866713]),
            {
              "class": 2,
              "system:index": "208"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88852665770976, 45.15643383539127]),
            {
              "class": 2,
              "system:index": "209"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.88831208098857, 45.15595719484257]),
            {
              "class": 2,
              "system:index": "210"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94371435332835, 45.12961333637757]),
            {
              "class": 2,
              "system:index": "211"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94349977660715, 45.12925001115019]),
            {
              "class": 2,
              "system:index": "212"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94204065490305, 45.1304308096773]),
            {
              "class": 2,
              "system:index": "213"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94156858611643, 45.1304308096773]),
            {
              "class": 2,
              "system:index": "214"
            })]),
    woody = /* color: #ff4809 */ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Point([-113.03313531188984, 44.3661616692714]),
            {
              "class": 0,
              "system:index": "0"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03350009231586, 44.36512622467024]),
            {
              "class": 0,
              "system:index": "1"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03034770757334, 44.36442104016869]),
            {
              "class": 0,
              "system:index": "2"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03025782769355, 44.36965081436767]),
            {
              "class": 0,
              "system:index": "3"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.02927077477607, 44.37277986040529]),
            {
              "class": 0,
              "system:index": "4"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05082624812763, 44.3716251030101]),
            {
              "class": 0,
              "system:index": "5"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05006986518543, 44.37211976282659]),
            {
              "class": 0,
              "system:index": "6"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0496138896529, 44.373210708673064]),
            {
              "class": 0,
              "system:index": "7"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05210297961872, 44.37529279679594]),
            {
              "class": 0,
              "system:index": "8"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05341189761799, 44.375219944133526]),
            {
              "class": 0,
              "system:index": "9"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05550402064961, 44.375233342740714]),
            {
              "class": 0,
              "system:index": "10"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05620675941151, 44.37616508290318]),
            {
              "class": 0,
              "system:index": "11"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05040245910328, 44.375938859288254]),
            {
              "class": 0,
              "system:index": "12"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04886287112873, 44.37745339055789]),
            {
              "class": 0,
              "system:index": "13"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04747348685902, 44.378191335161105]),
            {
              "class": 0,
              "system:index": "14"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0478489961211, 44.38009304687967]),
            {
              "class": 0,
              "system:index": "15"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0492169227187, 44.38070265510283]),
            {
              "class": 0,
              "system:index": "16"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.06204222914198, 44.38181840497723]),
            {
              "class": 0,
              "system:index": "17"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.06193494078138, 44.38234748367861]),
            {
              "class": 0,
              "system:index": "18"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.06199394937971, 44.38286888989486]),
            {
              "class": 0,
              "system:index": "19"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.02727777959545, 44.388585423059055]),
            {
              "class": 0,
              "system:index": "20"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.02693982125957, 44.39121769024849]),
            {
              "class": 0,
              "system:index": "21"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.00986887124937, 44.39651213794947]),
            {
              "class": 0,
              "system:index": "22"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03212482037068, 44.397499650032046]),
            {
              "class": 0,
              "system:index": "23"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03255448728191, 44.398802819450104]),
            {
              "class": 0,
              "system:index": "24"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0242664614259, 44.41293518117519]),
            {
              "class": 0,
              "system:index": "25"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04080047042312, 44.40641038535731]),
            {
              "class": 0,
              "system:index": "26"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03986706168594, 44.4077884221453]),
            {
              "class": 0,
              "system:index": "27"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04004408748092, 44.40834025920652]),
            {
              "class": 0,
              "system:index": "28"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04512965742065, 44.41098972918094]),
            {
              "class": 0,
              "system:index": "29"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03780186239196, 44.413969394335375]),
            {
              "class": 0,
              "system:index": "30"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0365304953189, 44.41525656775934]),
            {
              "class": 0,
              "system:index": "31"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03564000192596, 44.41675475522129]),
            {
              "class": 0,
              "system:index": "32"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0347949570315, 44.41750554428682]),
            {
              "class": 0,
              "system:index": "33"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03409758268762, 44.41804196347515]),
            {
              "class": 0,
              "system:index": "34"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03402784525323, 44.41773543882725]),
            {
              "class": 0,
              "system:index": "35"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0526248269444, 44.41125872408798]),
            {
              "class": 0,
              "system:index": "36"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04877853921704, 44.41161893027588]),
            {
              "class": 0,
              "system:index": "37"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04852641156964, 44.4125577551264]),
            {
              "class": 0,
              "system:index": "38"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04774320653729, 44.41363834303251]),
            {
              "class": 0,
              "system:index": "39"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04753935865216, 44.41464993914709]),
            {
              "class": 0,
              "system:index": "40"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04664350084118, 44.41700643041414]),
            {
              "class": 0,
              "system:index": "41"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04630554250531, 44.41787620075733]),
            {
              "class": 0,
              "system:index": "42"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04529166749768, 44.419577388168904]),
            {
              "class": 0,
              "system:index": "43"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04538255048178, 44.421151755784656]),
            {
              "class": 0,
              "system:index": "44"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04510500412648, 44.4239016633561]),
            {
              "class": 0,
              "system:index": "45"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04498698692983, 44.42511229866324]),
            {
              "class": 0,
              "system:index": "46"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04382290821736, 44.42556436491219]),
            {
              "class": 0,
              "system:index": "47"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04617337802895, 44.427140959387955]),
            {
              "class": 0,
              "system:index": "48"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04655425170907, 44.42822127780268]),
            {
              "class": 0,
              "system:index": "49"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0431585734077, 44.43561414172022]),
            {
              "class": 0,
              "system:index": "50"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30574743043474, 44.539227136266895]),
            {
              "class": 0,
              "system:index": "51"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30660037290147, 44.53926792916045]),
            {
              "class": 0,
              "system:index": "52"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30644760006054, 44.53881291948743]),
            {
              "class": 0,
              "system:index": "53"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30718253502542, 44.5409694596908]),
            {
              "class": 0,
              "system:index": "54"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3105460251301, 44.542024731715856]),
            {
              "class": 0,
              "system:index": "55"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31252013096505, 44.54206678926251]),
            {
              "class": 0,
              "system:index": "56"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3122710148495, 44.5428387242292]),
            {
              "class": 0,
              "system:index": "57"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32155145804103, 44.5444827463562]),
            {
              "class": 0,
              "system:index": "58"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32087017695125, 44.5460574387716]),
            {
              "class": 0,
              "system:index": "59"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31642843882258, 44.54597332941377]),
            {
              "class": 0,
              "system:index": "60"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31656254927333, 44.54689470257095]),
            {
              "class": 0,
              "system:index": "61"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32335390249904, 44.549198495684216]),
            {
              "class": 0,
              "system:index": "62"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32288183371242, 44.5495540283498]),
            {
              "class": 0,
              "system:index": "63"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31921257178004, 44.55093357953404]),
            {
              "class": 0,
              "system:index": "64"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31955053011592, 44.55229831370017]),
            {
              "class": 0,
              "system:index": "65"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32436964411168, 44.550907317067285]),
            {
              "class": 0,
              "system:index": "66"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32607552904516, 44.551893118617215]),
            {
              "class": 0,
              "system:index": "67"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3324267760304, 44.550136870010434]),
            {
              "class": 0,
              "system:index": "68"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.33105884943281, 44.54959401824488]),
            {
              "class": 0,
              "system:index": "69"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32845710668836, 44.549911319539554]),
            {
              "class": 0,
              "system:index": "70"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25699206762556, 44.55946286876109]),
            {
              "class": 0,
              "system:index": "71"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25686332159285, 44.55842320221577]),
            {
              "class": 0,
              "system:index": "72"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25911637716536, 44.559933005988796]),
            {
              "class": 0,
              "system:index": "73"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2591592925096, 44.56121726393121]),
            {
              "class": 0,
              "system:index": "74"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2593953269029, 44.56231803389065]),
            {
              "class": 0,
              "system:index": "75"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2593577759767, 44.563352151618595]),
            {
              "class": 0,
              "system:index": "76"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25811323099379, 44.56350120996654]),
            {
              "class": 0,
              "system:index": "77"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26069888048414, 44.56431146917731]),
            {
              "class": 0,
              "system:index": "78"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26162944762518, 44.566264226141726]),
            {
              "class": 0,
              "system:index": "79"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2636142822962, 44.56983364222061]),
            {
              "class": 0,
              "system:index": "80"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26627490148866, 44.574446205610414]),
            {
              "class": 0,
              "system:index": "81"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26527711973512, 44.574236034561515]),
            {
              "class": 0,
              "system:index": "82"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26425788030946, 44.57574499572337]),
            {
              "class": 0,
              "system:index": "83"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2666611395868, 44.57685066552503]),
            {
              "class": 0,
              "system:index": "84"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26619450089159, 44.584698705113276]),
            {
              "class": 0,
              "system:index": "85"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2669347905797, 44.58512661198542]),
            {
              "class": 0,
              "system:index": "86"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26712790962877, 44.585692055521044]),
            {
              "class": 0,
              "system:index": "87"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26036337849321, 44.58548192511959]),
            {
              "class": 0,
              "system:index": "88"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2598752164525, 44.58469488450199]),
            {
              "class": 0,
              "system:index": "89"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26784174256272, 44.581422355966964]),
            {
              "class": 0,
              "system:index": "90"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.02308302809838, 44.786976880105605]),
            {
              "class": 0,
              "system:index": "91"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.02603345801477, 44.78667229585215]),
            {
              "class": 0,
              "system:index": "92"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.03819385636976, 44.78472179982867]),
            {
              "class": 0,
              "system:index": "93"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0437835799568, 44.78334348942152]),
            {
              "class": 0,
              "system:index": "94"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05382205866947, 44.77772746267016]),
            {
              "class": 0,
              "system:index": "95"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05485202693119, 44.77623474534437]),
            {
              "class": 0,
              "system:index": "96"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05587126635685, 44.77466582738004]),
            {
              "class": 0,
              "system:index": "97"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05910064601078, 44.77246470071842]),
            {
              "class": 0,
              "system:index": "98"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.06375696086063, 44.76948657228842]),
            {
              "class": 0,
              "system:index": "99"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07711411251874, 44.76660797885563]),
            {
              "class": 0,
              "system:index": "100"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07450700535627, 44.766189027336246]),
            {
              "class": 0,
              "system:index": "101"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07876635327192, 44.76639469482546]),
            {
              "class": 0,
              "system:index": "102"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.09504536194731, 44.771656627512876]),
            {
              "class": 0,
              "system:index": "103"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.10954330690333, 44.7762915368711]),
            {
              "class": 0,
              "system:index": "104"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07056252154348, 44.80761087746893]),
            {
              "class": 0,
              "system:index": "105"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07587329539297, 44.808235046925]),
            {
              "class": 0,
              "system:index": "106"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07983223589895, 44.81020646462997]),
            {
              "class": 0,
              "system:index": "107"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0811733404064, 44.810822994559224]),
            {
              "class": 0,
              "system:index": "108"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96541243387178, 44.80970616411939]),
            {
              "class": 0,
              "system:index": "109"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96201139284089, 44.808769931281375]),
            {
              "class": 0,
              "system:index": "110"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96375482870057, 44.81020091517282]),
            {
              "class": 0,
              "system:index": "111"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96175926519349, 44.81061574163123]),
            {
              "class": 0,
              "system:index": "112"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96850233865693, 44.80988884192563]),
            {
              "class": 0,
              "system:index": "113"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97284751726106, 44.80814196171662]),
            {
              "class": 0,
              "system:index": "114"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95862516661141, 44.816480079002474]),
            {
              "class": 0,
              "system:index": "115"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95811018248055, 44.82020919814184]),
            {
              "class": 0,
              "system:index": "116"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95814236898873, 44.82110719488128]),
            {
              "class": 0,
              "system:index": "117"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95646867056344, 44.82431856029472]),
            {
              "class": 0,
              "system:index": "118"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00718108644375, 44.83385431531725]),
            {
              "class": 0,
              "system:index": "119"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00924102296719, 44.835786816479064]),
            {
              "class": 0,
              "system:index": "120"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98629375424036, 44.85186045987125]),
            {
              "class": 0,
              "system:index": "121"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9903170677627, 44.853237133443876]),
            {
              "class": 0,
              "system:index": "122"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9950403929269, 44.852853100897796]),
            {
              "class": 0,
              "system:index": "123"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99788353448268, 44.851875735591534]),
            {
              "class": 0,
              "system:index": "124"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00920245652553, 44.8532067737504]),
            {
              "class": 0,
              "system:index": "125"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97922422577078, 44.854691956451965]),
            {
              "class": 0,
              "system:index": "126"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97970434118444, 44.8554582204196]),
            {
              "class": 0,
              "system:index": "127"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99028617116537, 44.8638145747543]),
            {
              "class": 0,
              "system:index": "128"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96685439321126, 44.87833722931409]),
            {
              "class": 0,
              "system:index": "129"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96253067227926, 44.879158296130875]),
            {
              "class": 0,
              "system:index": "130"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9387027918869, 44.8777503181797]),
            {
              "class": 0,
              "system:index": "131"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93599912519988, 44.87664793028424]),
            {
              "class": 0,
              "system:index": "132"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93268391485748, 44.874116159517385]),
            {
              "class": 0,
              "system:index": "133"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92593547697601, 44.87465597584053]),
            {
              "class": 0,
              "system:index": "134"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9346151053482, 44.87133336405628]),
            {
              "class": 0,
              "system:index": "135"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93272683020172, 44.86552403501213]),
            {
              "class": 0,
              "system:index": "136"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93226549025115, 44.85896882629678]),
            {
              "class": 0,
              "system:index": "137"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92790958281097, 44.85793452566224]),
            {
              "class": 0,
              "system:index": "138"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98431634529241, 44.879759695571096]),
            {
              "class": 0,
              "system:index": "139"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98747062309393, 44.879725485045235]),
            {
              "class": 0,
              "system:index": "140"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98307419150558, 44.89298944870458]),
            {
              "class": 0,
              "system:index": "141"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9846084150621, 44.89325926896584]),
            {
              "class": 0,
              "system:index": "142"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98657715647903, 44.89425873298849]),
            {
              "class": 0,
              "system:index": "143"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98987090914932, 44.89612840611565]),
            {
              "class": 0,
              "system:index": "144"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9916841527579, 44.89722686977482]),
            {
              "class": 0,
              "system:index": "145"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00012941688826, 44.89919524347223]),
            {
              "class": 0,
              "system:index": "146"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.03222878442537, 44.910096351874614]),
            {
              "class": 0,
              "system:index": "147"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94955886505265, 44.89909764584211]),
            {
              "class": 0,
              "system:index": "148"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9465065111937, 44.898121065656554]),
            {
              "class": 0,
              "system:index": "149"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94227934978623, 44.89756246953321]),
            {
              "class": 0,
              "system:index": "150"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93974734447617, 44.897836068312486]),
            {
              "class": 0,
              "system:index": "151"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9673119720566, 44.90399349963366]),
            {
              "class": 0,
              "system:index": "152"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96800398198245, 44.904905388496424]),
            {
              "class": 0,
              "system:index": "153"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96467804280398, 44.90292581137435]),
            {
              "class": 0,
              "system:index": "154"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96039716188064, 44.906350125731144]),
            {
              "class": 0,
              "system:index": "155"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96194747869124, 44.90695802832526]),
            {
              "class": 0,
              "system:index": "156"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9820871407544, 44.923660231608544]),
            {
              "class": 0,
              "system:index": "157"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98323780842179, 44.92360705584588]),
            {
              "class": 0,
              "system:index": "158"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98688803562254, 44.923010907855954]),
            {
              "class": 0,
              "system:index": "159"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9898706520471, 44.92336794788034]),
            {
              "class": 0,
              "system:index": "160"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99669747863852, 44.92644447742496]),
            {
              "class": 0,
              "system:index": "161"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9976469806298, 44.92764845709198]),
            {
              "class": 0,
              "system:index": "162"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95964686019126, 44.92473856302142]),
            {
              "class": 0,
              "system:index": "163"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95783368689719, 44.92439292617212]),
            {
              "class": 0,
              "system:index": "164"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99000088755535, 45.0162383610209]),
            {
              "class": 0,
              "system:index": "165"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98906747881817, 45.015396499493505]),
            {
              "class": 0,
              "system:index": "166"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98854176585125, 45.01575296489426]),
            {
              "class": 0,
              "system:index": "167"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94438050341628, 45.0096077764183]),
            {
              "class": 0,
              "system:index": "168"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94702092182835, 45.01090482417343]),
            {
              "class": 0,
              "system:index": "169"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95062581074437, 45.01115512825377]),
            {
              "class": 0,
              "system:index": "170"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92234576731882, 45.09684083664905]),
            {
              "class": 0,
              "system:index": "171"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9312736856827, 45.096031133035424]),
            {
              "class": 0,
              "system:index": "172"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93440650581209, 45.09700434606206]),
            {
              "class": 0,
              "system:index": "173"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93670247672884, 45.0980267698969]),
            {
              "class": 0,
              "system:index": "174"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93897162555544, 45.0986591488091]),
            {
              "class": 0,
              "system:index": "175"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94442723869173, 45.09872730899228]),
            {
              "class": 0,
              "system:index": "176"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94799994109957, 45.09863264204921]),
            {
              "class": 0,
              "system:index": "177"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95272489845729, 45.09733835542165]),
            {
              "class": 0,
              "system:index": "178"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9307499518088, 45.18001336569213]),
            {
              "class": 0,
              "system:index": "179"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94186932861417, 45.1732641292695]),
            {
              "class": 0,
              "system:index": "180"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94557077705473, 45.17305991557834]),
            {
              "class": 0,
              "system:index": "181"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95381052314848, 45.17105555715332]),
            {
              "class": 0,
              "system:index": "182"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95962555229276, 45.17054878366263]),
            {
              "class": 0,
              "system:index": "183"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96457691013426, 45.17085511743113]),
            {
              "class": 0,
              "system:index": "184"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8096512714657, 45.278588450960996]),
            {
              "class": 0,
              "system:index": "185"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.80330027778366, 45.27920166853835]),
            {
              "class": 0,
              "system:index": "186"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7953287525914, 45.27871850653204]),
            {
              "class": 0,
              "system:index": "187"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7809199257634, 45.27642343081662]),
            {
              "class": 0,
              "system:index": "188"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.77532577575747, 45.272437793211736]),
            {
              "class": 0,
              "system:index": "189"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.80586968169222, 45.29790527266549]),
            {
              "class": 0,
              "system:index": "190"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.80350933775911, 45.30005609416961]),
            {
              "class": 0,
              "system:index": "191"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90650041014173, 45.303189645904546]),
            {
              "class": 0,
              "system:index": "192"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90699255206164, 45.30997339751466]),
            {
              "class": 0,
              "system:index": "193"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90889155604418, 45.313693097938135]),
            {
              "class": 0,
              "system:index": "194"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91016828753527, 45.31311214661642]),
            {
              "class": 0,
              "system:index": "195"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91459929682787, 45.31818961061919]),
            {
              "class": 0,
              "system:index": "196"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91494261958178, 45.32982909242225]),
            {
              "class": 0,
              "system:index": "197"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91454565264758, 45.331088698491904]),
            {
              "class": 0,
              "system:index": "198"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91618716456469, 45.33145827819085]),
            {
              "class": 0,
              "system:index": "199"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2323557949763, 44.514378968877374]),
            {
              "class": 0,
              "system:index": "200"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23057480819041, 44.514072949686245]),
            {
              "class": 0,
              "system:index": "201"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23626645572001, 44.52539500313988]),
            {
              "class": 0,
              "system:index": "202"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2564587784605, 44.55719109945184]),
            {
              "class": 0,
              "system:index": "203"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2562978459196, 44.556396030959036]),
            {
              "class": 0,
              "system:index": "204"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2555039120512, 44.555600951602784]),
            {
              "class": 0,
              "system:index": "205"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21492466050208, 44.65847909043748]),
            {
              "class": 0,
              "system:index": "206"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21413072663367, 44.65849053771315]),
            {
              "class": 0,
              "system:index": "207"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21349772530615, 44.6583493544885]),
            {
              "class": 0,
              "system:index": "208"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28736312250993, 44.70579425911119]),
            {
              "class": 0,
              "system:index": "209"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28857011656663, 44.70553118608405]),
            {
              "class": 0,
              "system:index": "210"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28943378786943, 44.70503553800218]),
            {
              "class": 0,
              "system:index": "211"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28617758612535, 44.70654153249292]),
            {
              "class": 0,
              "system:index": "212"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2843264160747, 44.710223235968925]),
            {
              "class": 0,
              "system:index": "213"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28338227850146, 44.71179390375673]),
            {
              "class": 0,
              "system:index": "214"
            })]),
    herb = /* color: #30e04a */ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Point([-113.92043578364428, 45.33154029531352]),
            {
              "class": 1,
              "system:index": "0"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92017428763727, 45.332089480991996]),
            {
              "class": 1,
              "system:index": "1"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90785763361387, 45.32146654582583]),
            {
              "class": 1,
              "system:index": "2"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90682323562676, 45.307547043444465]),
            {
              "class": 1,
              "system:index": "3"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9074240504461, 45.3069509377618]),
            {
              "class": 1,
              "system:index": "4"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.79314795447732, 45.28968968134071]),
            {
              "class": 1,
              "system:index": "5"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.79255786849404, 45.290101044684306]),
            {
              "class": 1,
              "system:index": "6"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.81367955315791, 45.286578029958825]),
            {
              "class": 1,
              "system:index": "7"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.81305728066646, 45.288110332981596]),
            {
              "class": 1,
              "system:index": "8"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87057373555996, 45.217770317078326]),
            {
              "class": 1,
              "system:index": "9"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92970956723165, 45.2023128697443]),
            {
              "class": 1,
              "system:index": "10"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92980076233816, 45.202588794387744]),
            {
              "class": 1,
              "system:index": "11"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93969840971529, 45.175363266950185]),
            {
              "class": 1,
              "system:index": "12"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91196822165688, 45.16517209298752]),
            {
              "class": 1,
              "system:index": "13"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9099168671186, 45.14668895437917]),
            {
              "class": 1,
              "system:index": "14"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90826462636542, 45.14626520164045]),
            {
              "class": 1,
              "system:index": "15"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.90827518375086, 45.12057320115226]),
            {
              "class": 1,
              "system:index": "16"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9088599053161, 45.11833755888743]),
            {
              "class": 1,
              "system:index": "17"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91851983931429, 45.11932359657597]),
            {
              "class": 1,
              "system:index": "18"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91921721365816, 45.12077744032916]),
            {
              "class": 1,
              "system:index": "19"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91900263693697, 45.12151933705378]),
            {
              "class": 1,
              "system:index": "20"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93816433813936, 45.13016441812796]),
            {
              "class": 1,
              "system:index": "21"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91279911401413, 45.10699605573069]),
            {
              "class": 1,
              "system:index": "22"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87576007670307, 45.084443553131564]),
            {
              "class": 1,
              "system:index": "23"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87597465342427, 45.08429962332421]),
            {
              "class": 1,
              "system:index": "24"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.86467744541918, 45.08073722303475]),
            {
              "class": 1,
              "system:index": "25"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.86515487862383, 45.08075426843582]),
            {
              "class": 1,
              "system:index": "26"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87667657781529, 45.07255962170932]),
            {
              "class": 1,
              "system:index": "27"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87569563427371, 45.07187088682542]),
            {
              "class": 1,
              "system:index": "28"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.87621761067763, 45.0713662100161]),
            {
              "class": 1,
              "system:index": "29"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91761732708612, 45.05662468458756]),
            {
              "class": 1,
              "system:index": "30"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98354355517611, 44.8931500636721]),
            {
              "class": 1,
              "system:index": "31"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98510996524081, 44.89338188050884]),
            {
              "class": 1,
              "system:index": "32"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98571614447818, 44.89394811770581]),
            {
              "class": 1,
              "system:index": "33"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9889079732059, 44.89538458705844]),
            {
              "class": 1,
              "system:index": "34"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9919335049747, 44.89689322126459]),
            {
              "class": 1,
              "system:index": "35"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99532918158755, 44.8984740146416]),
            {
              "class": 1,
              "system:index": "36"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99934713069186, 44.89929099418424]),
            {
              "class": 1,
              "system:index": "37"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00288361210153, 44.90146511257369]),
            {
              "class": 1,
              "system:index": "38"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92818766498583, 44.88548139982786]),
            {
              "class": 1,
              "system:index": "39"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05339719932697, 44.77787784109882]),
            {
              "class": 1,
              "system:index": "40"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05363323372028, 44.77907350719195]),
            {
              "class": 1,
              "system:index": "41"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.05614378135822, 44.77479336520667]),
            {
              "class": 1,
              "system:index": "42"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0588796345534, 44.77231804934725]),
            {
              "class": 1,
              "system:index": "43"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.06239869278095, 44.77023870203309]),
            {
              "class": 1,
              "system:index": "44"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0661001412215, 44.76811357742943]),
            {
              "class": 1,
              "system:index": "45"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.068170806581, 44.766940536093564]),
            {
              "class": 1,
              "system:index": "46"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07639982383868, 44.766582525226944]),
            {
              "class": 1,
              "system:index": "47"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.07556297462604, 44.76632353725943]),
            {
              "class": 1,
              "system:index": "48"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.99544301601351, 44.268752648560714]),
            {
              "class": 1,
              "system:index": "49"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05351667927506, 44.367526837647496]),
            {
              "class": 1,
              "system:index": "50"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05413895176652, 44.37547212186644]),
            {
              "class": 1,
              "system:index": "51"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05156403111222, 44.37554880854671]),
            {
              "class": 1,
              "system:index": "52"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03055697010758, 44.371637659807085]),
            {
              "class": 1,
              "system:index": "53"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.02993469761613, 44.37151495287558]),
            {
              "class": 1,
              "system:index": "54"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03195171879533, 44.36881533533831]),
            {
              "class": 1,
              "system:index": "55"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05510025837829, 44.37525014317471]),
            {
              "class": 1,
              "system:index": "56"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.06015890458038, 44.37546103210333]),
            {
              "class": 1,
              "system:index": "57"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0604163966458, 44.374487102342385]),
            {
              "class": 1,
              "system:index": "58"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04725747921874, 44.376155043033364]),
            {
              "class": 1,
              "system:index": "59"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04935496666839, 44.380460794420834]),
            {
              "class": 1,
              "system:index": "60"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05539353920082, 44.38151004427849]),
            {
              "class": 1,
              "system:index": "61"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.05138095451454, 44.38116498855816]),
            {
              "class": 1,
              "system:index": "62"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03969522266696, 44.40741828878282]),
            {
              "class": 1,
              "system:index": "63"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04018338470767, 44.407690377483966]),
            {
              "class": 1,
              "system:index": "64"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0397917821915, 44.407257334307445]),
            {
              "class": 1,
              "system:index": "65"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04033895283054, 44.40858327940006]),
            {
              "class": 1,
              "system:index": "66"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03687890320133, 44.41690641366831]),
            {
              "class": 1,
              "system:index": "67"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03636123686145, 44.417421764618894]),
            {
              "class": 1,
              "system:index": "68"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03677697925876, 44.41573775713817]),
            {
              "class": 1,
              "system:index": "69"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04286559372257, 44.41528944663163]),
            {
              "class": 1,
              "system:index": "70"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03679321747589, 44.416201287340975]),
            {
              "class": 1,
              "system:index": "71"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03516779881286, 44.41857112639011]),
            {
              "class": 1,
              "system:index": "72"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03639591204931, 44.41961315153583]),
            {
              "class": 1,
              "system:index": "73"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03639054763129, 44.41946755644086]),
            {
              "class": 1,
              "system:index": "74"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.03243697154333, 44.41947521934965]),
            {
              "class": 1,
              "system:index": "75"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.0446785734873, 44.42402643632737]),
            {
              "class": 1,
              "system:index": "76"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.04463565814306, 44.42537498764344]),
            {
              "class": 1,
              "system:index": "77"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.08609334586956, 44.41264589813853]),
            {
              "class": 1,
              "system:index": "78"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.08838395236828, 44.41077973226955]),
            {
              "class": 1,
              "system:index": "79"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.08626500724651, 44.4133083842945]),
            {
              "class": 1,
              "system:index": "80"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.09067644833613, 44.409946910337624]),
            {
              "class": 1,
              "system:index": "81"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.08943726777125, 44.409069357784205]),
            {
              "class": 1,
              "system:index": "82"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.09042968510676, 44.40787371914193]),
            {
              "class": 1,
              "system:index": "83"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.09305824994135, 44.40689266405327]),
            {
              "class": 1,
              "system:index": "84"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.08953919171381, 44.40707278086908]),
            {
              "class": 1,
              "system:index": "85"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.09149720429468, 44.406130035644956]),
            {
              "class": 1,
              "system:index": "86"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.09551117334323, 44.40374543151854]),
            {
              "class": 1,
              "system:index": "87"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.1082576273543, 44.395353885898714]),
            {
              "class": 1,
              "system:index": "88"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.11636862741534, 44.393284010681114]),
            {
              "class": 1,
              "system:index": "89"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.1159823893172, 44.39546121076534]),
            {
              "class": 1,
              "system:index": "90"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.11261353479449, 44.39422696291503]),
            {
              "class": 1,
              "system:index": "91"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.10821471201007, 44.39467160178635]),
            {
              "class": 1,
              "system:index": "92"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.1153708456618, 44.38984938954037]),
            {
              "class": 1,
              "system:index": "93"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.11183032976214, 44.39200371788231]),
            {
              "class": 1,
              "system:index": "94"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.14728364676616, 44.50145675792194]),
            {
              "class": 1,
              "system:index": "95"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.1403045389094, 44.49784865475633]),
            {
              "class": 1,
              "system:index": "96"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.14046010703227, 44.497554027776545]),
            {
              "class": 1,
              "system:index": "97"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27560233141199, 44.50138727067612]),
            {
              "class": 1,
              "system:index": "98"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2754413988711, 44.499045661145914]),
            {
              "class": 1,
              "system:index": "99"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2721828260675, 44.491516061176746]),
            {
              "class": 1,
              "system:index": "100"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27097534182379, 44.48966389431383]),
            {
              "class": 1,
              "system:index": "101"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2707607651026, 44.49284774810899]),
            {
              "class": 1,
              "system:index": "102"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27190875056097, 44.49496766974301]),
            {
              "class": 1,
              "system:index": "103"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27378839532601, 44.50540992755482]),
            {
              "class": 1,
              "system:index": "104"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27962488214241, 44.511010656805944]),
            {
              "class": 1,
              "system:index": "105"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28092307130562, 44.5130993155904]),
            {
              "class": 1,
              "system:index": "106"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28350872079598, 44.5152873540314]),
            {
              "class": 1,
              "system:index": "107"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28270405809151, 44.515861126739296]),
            {
              "class": 1,
              "system:index": "108"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28364819566475, 44.51689390337667]),
            {
              "class": 1,
              "system:index": "109"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28378767053353, 44.51740645980019]),
            {
              "class": 1,
              "system:index": "110"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2876178650068, 44.520405207025895]),
            {
              "class": 1,
              "system:index": "111"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29323977510201, 44.52511721227547]),
            {
              "class": 1,
              "system:index": "112"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3003529934095, 44.52933168626605]),
            {
              "class": 1,
              "system:index": "113"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30233782808052, 44.5313661507808]),
            {
              "class": 1,
              "system:index": "114"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30344289819466, 44.53342348811199]),
            {
              "class": 1,
              "system:index": "115"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30665094634988, 44.53716719560581]),
            {
              "class": 1,
              "system:index": "116"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31060988685586, 44.54007316070906]),
            {
              "class": 1,
              "system:index": "117"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31328136703469, 44.54276487267204]),
            {
              "class": 1,
              "system:index": "118"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31958992263772, 44.54478357497626]),
            {
              "class": 1,
              "system:index": "119"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.31832391998269, 44.54506649356996]),
            {
              "class": 1,
              "system:index": "120"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32384789468941, 44.55110666160822]),
            {
              "class": 1,
              "system:index": "121"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32407320024666, 44.551751754742604]),
            {
              "class": 1,
              "system:index": "122"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.329898958227, 44.55544443906646]),
            {
              "class": 1,
              "system:index": "123"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.33888972284493, 44.55602546122741]),
            {
              "class": 1,
              "system:index": "124"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32684413906382, 44.55914804684856]),
            {
              "class": 1,
              "system:index": "125"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.32805649753855, 44.56042467770554]),
            {
              "class": 1,
              "system:index": "126"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.33439543691672, 44.56534675550186]),
            {
              "class": 1,
              "system:index": "127"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42161575979843, 44.59733516647501]),
            {
              "class": 1,
              "system:index": "128"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42268864340438, 44.59680039473838]),
            {
              "class": 1,
              "system:index": "129"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42338601774826, 44.59636493411741]),
            {
              "class": 1,
              "system:index": "130"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42693189806594, 44.59140274448946]),
            {
              "class": 1,
              "system:index": "131"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42710892386093, 44.5908411767126]),
            {
              "class": 1,
              "system:index": "132"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43069235510482, 44.586050759468755]),
            {
              "class": 1,
              "system:index": "133"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43296686834945, 44.583844127318606]),
            {
              "class": 1,
              "system:index": "134"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43590656942978, 44.58135171456155]),
            {
              "class": 1,
              "system:index": "135"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.4386276445976, 44.57792151755154]),
            {
              "class": 1,
              "system:index": "136"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43723880312402, 44.597068105182075]),
            {
              "class": 1,
              "system:index": "137"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43742655775506, 44.59733167065304]),
            {
              "class": 1,
              "system:index": "138"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43668627411712, 44.59750975854346]),
            {
              "class": 1,
              "system:index": "139"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.43698668152679, 44.597257653238316]),
            {
              "class": 1,
              "system:index": "140"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44374726580769, 44.59945871891593]),
            {
              "class": 1,
              "system:index": "141"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36008744248969, 44.68484983315081]),
            {
              "class": 1,
              "system:index": "142"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36112813958746, 44.68640593368239]),
            {
              "class": 1,
              "system:index": "143"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36275892266852, 44.68645932854709]),
            {
              "class": 1,
              "system:index": "144"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36598830232245, 44.68961716586728]),
            {
              "class": 1,
              "system:index": "145"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36705045709235, 44.69327510687808]),
            {
              "class": 1,
              "system:index": "146"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36657838830573, 44.69422846612214]),
            {
              "class": 1,
              "system:index": "147"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.36924986848456, 44.694823354338226]),
            {
              "class": 1,
              "system:index": "148"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3728297248268, 44.69361662184392]),
            {
              "class": 1,
              "system:index": "149"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.37393479494094, 44.69606480234719]),
            {
              "class": 1,
              "system:index": "150"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.3754582896614, 44.698817928423296]),
            {
              "class": 1,
              "system:index": "151"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.37884860185622, 44.70103711204959]),
            {
              "class": 1,
              "system:index": "152"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.38456707147597, 44.701555671908146]),
            {
              "class": 1,
              "system:index": "153"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.38560776857375, 44.70244026330024]),
            {
              "class": 1,
              "system:index": "154"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.38754047527642, 44.70413376631834]),
            {
              "class": 1,
              "system:index": "155"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39193929806085, 44.706253618321085]),
            {
              "class": 1,
              "system:index": "156"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39616645946832, 44.70730589012372]),
            {
              "class": 1,
              "system:index": "157"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.395748034862, 44.70831239307901]),
            {
              "class": 1,
              "system:index": "158"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39751829281182, 44.709661994573544]),
            {
              "class": 1,
              "system:index": "159"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39976580150913, 44.71043603406464]),
            {
              "class": 1,
              "system:index": "160"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.40180428036045, 44.71249466006332]),
            {
              "class": 1,
              "system:index": "161"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.40290935047459, 44.71258615285261]),
            {
              "class": 1,
              "system:index": "162"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.40749056347202, 44.713661182292206]),
            {
              "class": 1,
              "system:index": "163"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.4106019259293, 44.71724446953272]),
            {
              "class": 1,
              "system:index": "164"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44554633126272, 44.735146164534385]),
            {
              "class": 1,
              "system:index": "165"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44933361039175, 44.73567204014949]),
            {
              "class": 1,
              "system:index": "166"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.45617195991392, 44.73580922430608]),
            {
              "class": 1,
              "system:index": "167"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.42994237645773, 44.75048267190224]),
            {
              "class": 1,
              "system:index": "168"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.45308810474978, 44.7516505053797]),
            {
              "class": 1,
              "system:index": "169"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44732350722057, 44.758644161535486]),
            {
              "class": 1,
              "system:index": "170"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44564980879528, 44.76364918284344]),
            {
              "class": 1,
              "system:index": "171"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44470567122204, 44.76436523871727]),
            {
              "class": 1,
              "system:index": "172"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44081110373241, 44.76998673070749]),
            {
              "class": 1,
              "system:index": "173"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.44060725584728, 44.77014668385654]),
            {
              "class": 1,
              "system:index": "174"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53566697293414, 44.75337382257502]),
            {
              "class": 1,
              "system:index": "175"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53641799145831, 44.75343477468436]),
            {
              "class": 1,
              "system:index": "176"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53892853909625, 44.75077567914402]),
            {
              "class": 1,
              "system:index": "177"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53978684598101, 44.748886046922756]),
            {
              "class": 1,
              "system:index": "178"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53881052189959, 44.74685919530839]),
            {
              "class": 1,
              "system:index": "179"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53742650204791, 44.748748893792325]),
            {
              "class": 1,
              "system:index": "180"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53499105626238, 44.7546233279673]),
            {
              "class": 1,
              "system:index": "181"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53312423878802, 44.7545014261915]),
            {
              "class": 1,
              "system:index": "182"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.52419784718646, 44.76083235952648]),
            {
              "class": 1,
              "system:index": "183"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.52313569241656, 44.76947818147447]),
            {
              "class": 1,
              "system:index": "184"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.52439096623553, 44.77007991298442]),
            {
              "class": 1,
              "system:index": "185"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53227666073931, 44.77212118329947]),
            {
              "class": 1,
              "system:index": "186"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53325279235027, 44.7740531919643]),
            {
              "class": 1,
              "system:index": "187"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53273780821941, 44.773885633321804]),
            {
              "class": 1,
              "system:index": "188"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53964181422374, 44.777548046785746]),
            {
              "class": 1,
              "system:index": "189"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23531331137862, 44.52208576430803]),
            {
              "class": 1,
              "system:index": "190"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23642911032881, 44.52379921543196]),
            {
              "class": 1,
              "system:index": "191"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23409022406783, 44.51993622980549]),
            {
              "class": 1,
              "system:index": "192"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23503436164107, 44.51962259002761]),
            {
              "class": 1,
              "system:index": "193"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.444029198044, 44.60157198668058]),
            {
              "class": 1,
              "system:index": "194"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.4406347492305, 44.61140487903254]),
            {
              "class": 1,
              "system:index": "195"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.4376038280041, 44.616131243114694]),
            {
              "class": 1,
              "system:index": "196"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95742141081142, 44.950261530817855]),
            {
              "class": 1,
              "system:index": "197"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96148904270183, 44.944019732585446]),
            {
              "class": 1,
              "system:index": "198"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39429558686518, 44.58013212747672]),
            {
              "class": 1,
              "system:index": "199"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39329561677451, 44.57593334634811]),
            {
              "class": 1,
              "system:index": "200"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39230856385703, 44.57396158033949]),
            {
              "class": 1,
              "system:index": "201"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.39416465249533, 44.571103165174]),
            {
              "class": 1,
              "system:index": "202"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.40076288667197, 44.562488856042265]),
            {
              "class": 1,
              "system:index": "203"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.74994466371191, 45.08607275030614]),
            {
              "class": 1,
              "system:index": "204"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.74942431516303, 45.086186375604655]),
            {
              "class": 1,
              "system:index": "205"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.74892005986823, 45.088080097305955]),
            {
              "class": 1,
              "system:index": "206"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.76490042587366, 45.09004976062694]),
            {
              "class": 1,
              "system:index": "207"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7708334722146, 45.088167466352445]),
            {
              "class": 1,
              "system:index": "208"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.84743003116255, 45.10100823828383]),
            {
              "class": 1,
              "system:index": "209"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.84781626926069, 45.103764760710305]),
            {
              "class": 1,
              "system:index": "210"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.84783772693281, 45.10474920075096]),
            {
              "class": 1,
              "system:index": "211"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.69004057998406, 45.118673677160004]),
            {
              "class": 1,
              "system:index": "212"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.690571657369, 45.118102086213106]),
            {
              "class": 1,
              "system:index": "213"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.68967579955803, 45.119305827366915]),
            {
              "class": 1,
              "system:index": "214"
            })]),
    geometry = 
    /* color: #d63000 */
    /* shown: false */
    /* displayProperties: [
      {
        "type": "rectangle"
      }
    ] */
    ee.Geometry.Polygon(
        [[[-114.30854836267648, 45.61540876057385],
          [-114.30854836267648, 44.19148776780534],
          [-112.82539406580148, 44.19148776780534],
          [-112.82539406580148, 45.61540876057385]]], null, false),
    upland = /* color: #885151 */ee.FeatureCollection(
        [ee.Feature(
            ee.Geometry.Point([-112.97820201043412, 44.274341619385986]),
            {
              "class": 3,
              "system:index": "0"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97827711228653, 44.27520966822398]),
            {
              "class": 3,
              "system:index": "1"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.9795860302858, 44.274748758933214]),
            {
              "class": 3,
              "system:index": "2"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97513356332108, 44.27427248220157]),
            {
              "class": 3,
              "system:index": "3"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97545542840287, 44.27478716817885]),
            {
              "class": 3,
              "system:index": "4"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97592749718949, 44.2753786673915]),
            {
              "class": 3,
              "system:index": "5"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97933414551767, 44.304836129819485]),
            {
              "class": 3,
              "system:index": "6"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97830417725595, 44.304275639165596]),
            {
              "class": 3,
              "system:index": "7"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97894790741952, 44.30440616486434]),
            {
              "class": 3,
              "system:index": "8"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.97692015740427, 44.30363836246881]),
            {
              "class": 3,
              "system:index": "9"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.98112586113962, 44.30477470644727]),
            {
              "class": 3,
              "system:index": "10"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.98347547623666, 44.304482944551594]),
            {
              "class": 3,
              "system:index": "11"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.9867425947352, 44.30510238667646]),
            {
              "class": 3,
              "system:index": "12"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.99184832464243, 44.30651489430589]),
            {
              "class": 3,
              "system:index": "13"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.99342928640061, 44.30791541879899]),
            {
              "class": 3,
              "system:index": "14"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.9953309788895, 44.309846833275955]),
            {
              "class": 3,
              "system:index": "15"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.9988035228313, 44.310556592479735]),
            {
              "class": 3,
              "system:index": "16"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.99432959819445, 44.31155461600052]),
            {
              "class": 3,
              "system:index": "17"
            }),
        ee.Feature(
            ee.Geometry.Point([-112.99804617605365, 44.31293904946395]),
            {
              "class": 3,
              "system:index": "18"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.16868963697131, 44.47757961548481]),
            {
              "class": 3,
              "system:index": "19"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.16534224012072, 44.47947809631522]),
            {
              "class": 3,
              "system:index": "20"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.16328230359728, 44.47947809631522]),
            {
              "class": 3,
              "system:index": "21"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.16165152051623, 44.47806955193523]),
            {
              "class": 3,
              "system:index": "22"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.15941992261584, 44.47494613658327]),
            {
              "class": 3,
              "system:index": "23"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.1559008643883, 44.473476236214815]),
            {
              "class": 3,
              "system:index": "24"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.14577284314807, 44.472435034386464]),
            {
              "class": 3,
              "system:index": "25"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.13779058911975, 44.47561982854534]),
            {
              "class": 3,
              "system:index": "26"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.13512983777697, 44.47084257212963]),
            {
              "class": 3,
              "system:index": "27"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.18053427198107, 44.46906631296651]),
            {
              "class": 3,
              "system:index": "28"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21788635408846, 44.4856155366816]),
            {
              "class": 3,
              "system:index": "29"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23041763460604, 44.48371725555937]),
            {
              "class": 3,
              "system:index": "30"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.22623283484025, 44.49548482799923]),
            {
              "class": 3,
              "system:index": "31"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23867828466935, 44.498056159051785]),
            {
              "class": 3,
              "system:index": "32"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26069951603432, 44.511584390127126]),
            {
              "class": 3,
              "system:index": "33"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2578671033146, 44.514644640401144]),
            {
              "class": 3,
              "system:index": "34"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24250341007729, 44.51513426553222]),
            {
              "class": 3,
              "system:index": "35"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21409345219155, 44.51391019499102]),
            {
              "class": 3,
              "system:index": "36"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.19915891239663, 44.512012834839496]),
            {
              "class": 3,
              "system:index": "37"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.19366574833413, 44.51495065659015]),
            {
              "class": 3,
              "system:index": "38"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.22409712259675, 44.53647241971309]),
            {
              "class": 3,
              "system:index": "39"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.22667204325104, 44.540326661717195]),
            {
              "class": 3,
              "system:index": "40"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.20950590555573, 44.53065998506365]),
            {
              "class": 3,
              "system:index": "41"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24426733438874, 44.53225081431095]),
            {
              "class": 3,
              "system:index": "42"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27885710184479, 44.53414751546843]),
            {
              "class": 3,
              "system:index": "43"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29499327127839, 44.53659478052321]),
            {
              "class": 3,
              "system:index": "44"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24546159121505, 44.55659731524516]),
            {
              "class": 3,
              "system:index": "45"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27335656496993, 44.55629151783615]),
            {
              "class": 3,
              "system:index": "46"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29206765505782, 44.55904363666457]),
            {
              "class": 3,
              "system:index": "47"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2931833732286, 44.57175055238139]),
            {
              "class": 3,
              "system:index": "48"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2816159151795, 44.579247711918896]),
            {
              "class": 3,
              "system:index": "49"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28024262416388, 44.57655768725787]),
            {
              "class": 3,
              "system:index": "50"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27268952357794, 44.577169067426844]),
            {
              "class": 3,
              "system:index": "51"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30642098414923, 44.57411210230064]),
            {
              "class": 3,
              "system:index": "52"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21842668227843, 44.59336924924487]),
            {
              "class": 3,
              "system:index": "53"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.20992944411925, 44.593308128552856]),
            {
              "class": 3,
              "system:index": "54"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21499345473937, 44.59501948363013]),
            {
              "class": 3,
              "system:index": "55"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29844487000268, 44.60940880508696]),
            {
              "class": 3,
              "system:index": "56"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28780186463159, 44.60702570845603]),
            {
              "class": 3,
              "system:index": "57"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30668461609643, 44.60158699358212]),
            {
              "class": 3,
              "system:index": "58"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.28502287716283, 44.62449215352972]),
            {
              "class": 3,
              "system:index": "59"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27429404110326, 44.62498085481834]),
            {
              "class": 3,
              "system:index": "60"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25635542721166, 44.629562229318466]),
            {
              "class": 3,
              "system:index": "61"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.25343718380346, 44.62852381612329]),
            {
              "class": 3,
              "system:index": "62"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24983229488744, 44.626446933999546]),
            {
              "class": 3,
              "system:index": "63"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24322333187474, 44.6251641167409]),
            {
              "class": 3,
              "system:index": "64"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23412527889623, 44.62284275630699]),
            {
              "class": 3,
              "system:index": "65"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26352830725368, 44.63820468303648]),
            {
              "class": 3,
              "system:index": "66"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24833627539333, 44.64015900169952]),
            {
              "class": 3,
              "system:index": "67"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27674623327907, 44.64801209470216]),
            {
              "class": 3,
              "system:index": "68"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2400389381976, 44.66708936939577]),
            {
              "class": 3,
              "system:index": "69"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23325831380795, 44.67191157182042]),
            {
              "class": 3,
              "system:index": "70"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.21566302267026, 44.66238886195517]),
            {
              "class": 3,
              "system:index": "71"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30971712346484, 44.675465737980545]),
            {
              "class": 3,
              "system:index": "72"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30679888005663, 44.67351260892322]),
            {
              "class": 3,
              "system:index": "73"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.30182070012499, 44.67613711089077]),
            {
              "class": 3,
              "system:index": "74"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29692835088183, 44.68425400412747]),
            {
              "class": 3,
              "system:index": "75"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.24199671025683, 44.679921067273]),
            {
              "class": 3,
              "system:index": "76"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.23693269963671, 44.67741880121075]),
            {
              "class": 3,
              "system:index": "77"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.2256030487578, 44.6771746719069]),
            {
              "class": 3,
              "system:index": "78"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.26418134014187, 44.69243447767955]),
            {
              "class": 3,
              "system:index": "79"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.29891904674697, 44.698296965625254]),
            {
              "class": 3,
              "system:index": "80"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.27488645397354, 44.70189645569561]),
            {
              "class": 3,
              "system:index": "81"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.47698600144325, 44.7696945039867]),
            {
              "class": 3,
              "system:index": "82"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.49174887986122, 44.77603138859317]),
            {
              "class": 3,
              "system:index": "83"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.54805381150184, 44.76945076376985]),
            {
              "class": 3,
              "system:index": "84"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.55869681687294, 44.77335048380998]),
            {
              "class": 3,
              "system:index": "85"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.55320365281044, 44.758968961407845]),
            {
              "class": 3,
              "system:index": "86"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.52402121872841, 44.800003132612034]),
            {
              "class": 3,
              "system:index": "87"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.53569419236122, 44.800003132612034]),
            {
              "class": 3,
              "system:index": "88"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.58341605515419, 44.79391251702477]),
            {
              "class": 3,
              "system:index": "89"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.59302909226356, 44.798785060924004]),
            {
              "class": 3,
              "system:index": "90"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.52402121872841, 44.81997583931249]),
            {
              "class": 3,
              "system:index": "91"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.55148703904091, 44.82387214639834]),
            {
              "class": 3,
              "system:index": "92"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.55869681687294, 44.830446567506264]),
            {
              "class": 3,
              "system:index": "93"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.61637503952919, 44.820706416948696]),
            {
              "class": 3,
              "system:index": "94"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.6222115263456, 44.82265457872037]),
            {
              "class": 3,
              "system:index": "95"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.63045127243934, 44.830933531833125]),
            {
              "class": 3,
              "system:index": "96"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.63010794968544, 44.82533319366828]),
            {
              "class": 3,
              "system:index": "97"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.58581931443153, 44.845540548448014]),
            {
              "class": 3,
              "system:index": "98"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.56933982224403, 44.84505370755291]),
            {
              "class": 3,
              "system:index": "99"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.64590079636513, 44.87231046149814]),
            {
              "class": 3,
              "system:index": "100"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65894706101356, 44.94477005855888]),
            {
              "class": 3,
              "system:index": "101"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.67267997116981, 44.945924295754814]),
            {
              "class": 3,
              "system:index": "102"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.69061858506142, 44.94598504443803]),
            {
              "class": 3,
              "system:index": "103"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.69671256394325, 44.95041952467031]),
            {
              "class": 3,
              "system:index": "104"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7009182676786, 44.94671402362214]),
            {
              "class": 3,
              "system:index": "105"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.68237883896767, 44.940638914219285]),
            {
              "class": 3,
              "system:index": "106"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.61380011887489, 44.94185398753048]),
            {
              "class": 3,
              "system:index": "107"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.61294181199013, 44.94574204931943]),
            {
              "class": 3,
              "system:index": "108"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.60727698655067, 44.95224181448147]),
            {
              "class": 3,
              "system:index": "109"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.62023742051063, 44.957040234190856]),
            {
              "class": 3,
              "system:index": "110"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.61869246811806, 44.95934819189762]),
            {
              "class": 3,
              "system:index": "111"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.62306983323036, 44.96025920227991]),
            {
              "class": 3,
              "system:index": "112"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.6650410398954, 44.96208117964982]),
            {
              "class": 3,
              "system:index": "113"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.57809455246864, 44.987461378223564]),
            {
              "class": 3,
              "system:index": "114"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.59594733567177, 44.99219615254488]),
            {
              "class": 3,
              "system:index": "115"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.63285453171669, 45.006640866947805]),
            {
              "class": 3,
              "system:index": "116"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.70323569626747, 45.024115315757186]),
            {
              "class": 3,
              "system:index": "117"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.64143760056434, 45.04777013228882]),
            {
              "class": 3,
              "system:index": "118"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.60624701828895, 45.03697504440268]),
            {
              "class": 3,
              "system:index": "119"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.60161216111122, 45.0413418421303]),
            {
              "class": 3,
              "system:index": "120"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.58684928269325, 45.04449543322359]),
            {
              "class": 3,
              "system:index": "121"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65568549485145, 45.03806677507871]),
            {
              "class": 3,
              "system:index": "122"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.65911872239052, 45.043888986898516]),
            {
              "class": 3,
              "system:index": "123"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.71954352707802, 45.03891588453728]),
            {
              "class": 3,
              "system:index": "124"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.73636634201942, 45.03382103877825]),
            {
              "class": 3,
              "system:index": "125"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.7294998869413, 45.05698679723421]),
            {
              "class": 3,
              "system:index": "126"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.67868811936317, 45.07093024237632]),
            {
              "class": 3,
              "system:index": "127"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.726924966287, 45.07771886267603]),
            {
              "class": 3,
              "system:index": "128"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.74958426804481, 45.08293100591393]),
            {
              "class": 3,
              "system:index": "129"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.76451880783974, 45.0845066765451]),
            {
              "class": 3,
              "system:index": "130"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.78013999314247, 45.08874865069799]),
            {
              "class": 3,
              "system:index": "131"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.79438788742958, 45.092747937802926]),
            {
              "class": 3,
              "system:index": "132"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.80316598736613, 45.10010493061709]),
            {
              "class": 3,
              "system:index": "133"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.81707055889933, 45.10155896005921]),
            {
              "class": 3,
              "system:index": "134"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.81689889752238, 45.10652661459086]),
            {
              "class": 3,
              "system:index": "135"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.83320672833293, 45.10592082620627]),
            {
              "class": 3,
              "system:index": "136"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.83921487652628, 45.1043457463196]),
            {
              "class": 3,
              "system:index": "137"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.85535104595988, 45.09501553595929]),
            {
              "class": 3,
              "system:index": "138"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89402836745349, 45.08265368897865]),
            {
              "class": 3,
              "system:index": "139"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.8917967695531, 45.069319286973965]),
            {
              "class": 3,
              "system:index": "140"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94695501363336, 45.0671369976787]),
            {
              "class": 3,
              "system:index": "141"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94609670674859, 45.06446964205944]),
            {
              "class": 3,
              "system:index": "142"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93820028340875, 45.06131715222783]),
            {
              "class": 3,
              "system:index": "143"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93390874898492, 45.07259256469651]),
            {
              "class": 3,
              "system:index": "144"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93699865377008, 45.0570731414052]),
            {
              "class": 3,
              "system:index": "145"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.89837484395562, 45.04967568388443]),
            {
              "class": 3,
              "system:index": "146"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94146184957086, 45.044581796254406]),
            {
              "class": 3,
              "system:index": "147"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93356542623101, 45.03026797014761]),
            {
              "class": 3,
              "system:index": "148"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93236379659234, 45.02735624529247]),
            {
              "class": 3,
              "system:index": "149"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.91468267476617, 45.0161932611768]),
            {
              "class": 3,
              "system:index": "150"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94695501363336, 45.00065853212024]),
            {
              "class": 3,
              "system:index": "151"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.92172079122125, 45.00417848831606]),
            {
              "class": 3,
              "system:index": "152"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93717031514703, 44.986576544675536]),
            {
              "class": 3,
              "system:index": "153"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95004491841851, 44.98864050060955]),
            {
              "class": 3,
              "system:index": "154"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95004491841851, 44.97273397883493]),
            {
              "class": 3,
              "system:index": "155"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9522765163189, 44.95900954088894]),
            {
              "class": 3,
              "system:index": "156"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9383719447857, 44.96192474040224]),
            {
              "class": 3,
              "system:index": "157"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9522765163189, 44.94443132151845]),
            {
              "class": 3,
              "system:index": "158"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95141820943414, 44.939814113410755]),
            {
              "class": 3,
              "system:index": "159"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97888402974664, 44.934467408539916]),
            {
              "class": 3,
              "system:index": "160"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98351888692437, 44.933130654533436]),
            {
              "class": 3,
              "system:index": "161"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98849706685601, 44.935682612453824]),
            {
              "class": 3,
              "system:index": "162"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95004491841851, 44.93543957372825]),
            {
              "class": 3,
              "system:index": "163"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95347814595758, 44.927540255197194]),
            {
              "class": 3,
              "system:index": "164"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0072081569439, 44.91684406203382]),
            {
              "class": 3,
              "system:index": "165"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9991400722271, 44.91064422042789]),
            {
              "class": 3,
              "system:index": "166"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98849706685601, 44.90942848677958]),
            {
              "class": 3,
              "system:index": "167"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98575048482476, 44.90553796629866]),
            {
              "class": 3,
              "system:index": "168"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97665243184625, 44.910279503033486]),
            {
              "class": 3,
              "system:index": "169"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94043188130914, 44.91538533594726]),
            {
              "class": 3,
              "system:index": "170"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94077520406304, 44.907726416470354]),
            {
              "class": 3,
              "system:index": "171"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93648366963922, 44.90553796629866]),
            {
              "class": 3,
              "system:index": "172"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.94815664327203, 44.90456529503407]),
            {
              "class": 3,
              "system:index": "173"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.03072576558648, 44.90176877347368]),
            {
              "class": 3,
              "system:index": "174"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0152762416607, 44.90116081600337]),
            {
              "class": 3,
              "system:index": "175"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00480489766656, 44.897877734574074]),
            {
              "class": 3,
              "system:index": "176"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98077230489312, 44.8874192627429]),
            {
              "class": 3,
              "system:index": "177"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95416479146539, 44.88571654081071]),
            {
              "class": 3,
              "system:index": "178"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93888692891656, 44.88583816547735]),
            {
              "class": 3,
              "system:index": "179"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98798208272515, 44.87562079704972]),
            {
              "class": 3,
              "system:index": "180"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98128728902398, 44.87452597138609]),
            {
              "class": 3,
              "system:index": "181"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95382146871148, 44.8699031445038]),
            {
              "class": 3,
              "system:index": "182"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93923025167047, 44.87233625757112]),
            {
              "class": 3,
              "system:index": "183"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99931173360406, 44.86114308577783]),
            {
              "class": 3,
              "system:index": "184"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99965505635797, 44.865523281774486]),
            {
              "class": 3,
              "system:index": "185"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96240453755914, 44.85724929844544]),
            {
              "class": 3,
              "system:index": "186"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96017293965875, 44.86029134229955]),
            {
              "class": 3,
              "system:index": "187"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95639638936578, 44.85931790575247]),
            {
              "class": 3,
              "system:index": "188"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.93957357442437, 44.854572166907104]),
            {
              "class": 3,
              "system:index": "189"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99742345845758, 44.84398410927354]),
            {
              "class": 3,
              "system:index": "190"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9874670985943, 44.84447095920797]),
            {
              "class": 3,
              "system:index": "191"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96223287618218, 44.838750212628916]),
            {
              "class": 3,
              "system:index": "192"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99862508809625, 44.83607222118137]),
            {
              "class": 3,
              "system:index": "193"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9962218288189, 44.832055000647024]),
            {
              "class": 3,
              "system:index": "194"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.0156195644146, 44.83071586490545]),
            {
              "class": 3,
              "system:index": "195"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.99776678121148, 44.827672258866684]),
            {
              "class": 3,
              "system:index": "196"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9768240932232, 44.81549622753186]),
            {
              "class": 3,
              "system:index": "197"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.9793990138775, 44.82012342236007]),
            {
              "class": 3,
              "system:index": "198"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.98506383931695, 44.80831116289007]),
            {
              "class": 3,
              "system:index": "199"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96738271749078, 44.81768810294771]),
            {
              "class": 3,
              "system:index": "200"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96824102437554, 44.81525268067602]),
            {
              "class": 3,
              "system:index": "201"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.97047262227593, 44.80015076677432]),
            {
              "class": 3,
              "system:index": "202"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.01836614644586, 44.7991763139745]),
            {
              "class": 3,
              "system:index": "203"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.02900915181695, 44.802221424330206]),
            {
              "class": 3,
              "system:index": "204"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.03707723653375, 44.80149061250224]),
            {
              "class": 3,
              "system:index": "205"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.03965215718804, 44.79369471032736]),
            {
              "class": 3,
              "system:index": "206"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.01115636861383, 44.78565408304938]),
            {
              "class": 3,
              "system:index": "207"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.02214269673883, 44.779196404778574]),
            {
              "class": 3,
              "system:index": "208"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00617818868218, 44.77261614062694]),
            {
              "class": 3,
              "system:index": "209"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00686483419, 44.76579137098019]),
            {
              "class": 3,
              "system:index": "210"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.95673971211968, 44.75287084916056]),
            {
              "class": 3,
              "system:index": "211"
            }),
        ee.Feature(
            ee.Geometry.Point([-113.96600942647515, 44.75006695823563]),
            {
              "class": 3,
              "system:index": "212"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.00772314107476, 44.748847832796066]),
            {
              "class": 3,
              "system:index": "213"
            }),
        ee.Feature(
            ee.Geometry.Point([-114.04136877095758, 44.748725918837806]),
            {
              "class": 3,
              "system:index": "214"
            })]),
    table2 = ee.FeatureCollection("projects/uiriparian/assets/SFO_monitoring_sites_shapes"),
    pastures = ee.FeatureCollection("projects/uiriparian/assets/SFO_allPastures"),
    riparian = ee.FeatureCollection("projects/uiriparian/assets/SFO_riparianAreas");
Map.addLayer(table)
var trainingFeatures = ee.FeatureCollection([
  woody,herb,water,upland
]).flatten();

function addNDVI(image){
  var nir = image.select('N');
  var red = image.select('R');
  var one = nir.subtract(red);
  var two = nir.add(red);
  var ndvi = one.divide(two).rename('NDVI');
  return image.addBands(ndvi);
}

function addGRVI(image){
  var green = image.select('G');
  var red = image.select('R');
  //var blue = image.select('B')
  var one = green.subtract(red);
  var two = green.add(red);
  var grvi = one.divide(two).rename('GRVI');
  return image.addBands(grvi);
}

function addVARI(image){
  var green = image.select('G');
  var red = image.select('R');
  var blue = image.select('B')
  var one = green.subtract(red);
  var two = green.add(red).subtract(blue);
  var vari = one.divide(two).rename('VARI');
  return image.addBands(vari);
}

function addTGI(image){
  var green = image.select('G');
  var red = image.select('R');
  var blue = image.select('B')
  var one = red.multiply(0.39);
  var two = blue.multiply(0.61);
  var tgi = green.subtract(one).subtract(two).rename('TGI');
  return image.addBands(tgi);
}

var img = ee.ImageCollection('USDA/NAIP/DOQQ')
                  .filter(ee.Filter.date('2004-01-01', '2023-12-31'))
                  .map(addNDVI)
                  .map(addGRVI)
                  .map(addVARI)
                  .map(addTGI);

var img2023 = img
  .filterDate('2009-01-01','2009-12-31')
  .filterDate('2009-07-15','2009-07-21')
  .mosaic();

img2023 = img2023.clip(geometry)
//img2023 = img2023.reproject(img2023.select('N').projection())
//Map.addLayer(img2023.select('NDVI'))
//img2023 = img2023.updateMask(img2023.select(['N','R','G','B']).reduce(ee.Reducer.min()).mask());
Map.addLayer(img2023.select(['N','R','G']))

// Get the NIR band.
var nir = img2023.select('N');
// Define a neighborhood with a kernel.
var square = ee.Kernel.square({radius: 4});
// Compute entropy and display.
var entropy = nir.entropy(square);
// Compute the gray-level co-occurrence matrix (GLCM), get contrast.
var glcm = nir.glcmTexture({size: 4});
var contrast = glcm.select('N_contrast');
// Create a list of weights for a 9x9 kernel.
var row = [1, 1, 1, 1, 1, 1, 1, 1, 1];
// The center of the kernel is zero.
var centerRow = [1, 1, 1, 1, 0, 1, 1, 1, 1];
// Assemble a list of lists: the 9x9 kernel weights as a 2-D matrix.
var rows = [row, row, row, row, centerRow, row, row, row, row];
// Create the kernel from the weights.
// Non-zero weights represent the spatial neighborhood.
var kernel = ee.Kernel.fixed(9, 9, rows, -4, -4, false);
// Convert the neighborhood into multiple bands.
var neighs = nir.neighborhoodToBands(kernel);
// Compute local Geary's C, a measure of spatial association.
var gearys = nir.subtract(neighs).pow(2).reduce(ee.Reducer.sum())
             .divide(Math.pow(9, 2));
gearys = gearys.rename('gearys')
contrast = contrast.rename('contrast')
entropy = entropy.rename('entropy')
img2023 = img2023.addBands([entropy,contrast,gearys]);

var predictionBands = ['B','G','R','N','NDVI','entropy','contrast','VARI','TGI']
// get spectral information from training areas

var training = img2023.select(predictionBands).sampleRegions({
  collection: trainingFeatures,
  properties: ['class'],
  scale: 1.0
})

// Get the number of features
var count = training.size();
// Create a list [1, 2, 3, ..., count]
var ids = ee.List.sequence(1, count);
// Zip the list with the features and map to assign IDs
var trainingWithID = ee.FeatureCollection(
  ids.zip(training.toList(count)).map(function(el) {
    el = ee.List(el);
    var id = ee.Number(el.get(0));
    var feat = ee.Feature(el.get(1));
    return feat.set('ID', id);
  })
);
var limited = trainingWithID.select('ID')
var withRandom = limited.randomColumn('random');
var innerJoin = ee.Join.inner();
var filterEq = ee.Filter.equals({
  leftField: 'ID',
  rightField: 'ID'
});
var trainingWithRandom = innerJoin.apply(trainingWithID, withRandom,filterEq);

function cleanJoin(feature){
  return ee.Feature(feature.get('primary')).copyProperties(feature.get('secondary'));
}
//print(trainingWithRandom.map(cleanJoin))

var TWR = trainingWithRandom.map(cleanJoin)

var split = 0.8;  // Roughly 80% training, 20% testing.
var trainingPartition = TWR.filter(ee.Filter.lt('random', split));
var testingPartition = TWR.filter(ee.Filter.gte('random', split));
print(testingPartition)
// Trained with 80% of our data.
var trainedClassifier = ee.Classifier.smileRandomForest({
  numberOfTrees:100,
  bagFraction:0.7,
  seed:123
}).train({
  features: trainingPartition,
  classProperty: 'class',
  inputProperties: predictionBands
});

// Classify the test FeatureCollection.
var test = testingPartition.classify(trainedClassifier);

// Print the confusion matrix.
var confusionMatrix = test.errorMatrix('class', 'classification');
//print('Confusion Matrix', confusionMatrix);
//print('Validation overall accuracy: ', confusionMatrix.accuracy());
//print('OOB Error:',trainedClassifier.explain().getNumber('outOfBagErrorEstimate'))
//print('Variable Importance',ee.Feature(null, ee.Dictionary(trainedClassifier.explain()).get('importance')));



var trainedClassifierFull = ee.Classifier.smileRandomForest({
  numberOfTrees:100,
  bagFraction:0.7,
  seed:123
}).train({
  features: training,
  classProperty: 'class',
  inputProperties: predictionBands
});
//riparian = ee.Feature(riparian.geometry())
var imgRiparian = img2023.clipToCollection(riparian)
//var pasturesRiparian = pastures.map(function(feature){
//  return feature.intersection(riparian,1);
//});
var riparianAreasClassified = imgRiparian.classify(trainedClassifier);
var dict = ee.Dictionary({
  0:'woody',
  1:'herb',
  2:'water',
  3:'upland'
});

var sitesWithCover = pastures.map(function(feat){
  var counts = riparianAreasClassified.reduceRegion({
  reducer: ee.Reducer.frequencyHistogram(),
  geometry: feat.geometry(),
  scale: 1.0,
  maxPixels: 1e13
  }).values().get(0);

  var counts_keys = ee.Dictionary(counts).keys();
  var counts_values = ee.Dictionary(counts).values();
  var totPixels = counts_values.reduce(ee.Reducer.sum());
  var new_counts_keys = counts_keys.map(function(ele) {
    return dict.get(ele);
  });

  var new_counts_values = counts_values.map(function(ele) {
    return ee.Number(ele).divide(totPixels);
  });

  var new_dict = ee.Dictionary.fromLists(new_counts_keys, new_counts_values);

  return feat.set(new_dict);
});

print(sitesWithCover);

Export.table.toDrive({
  collection: sitesWithCover,
  folder: 'NAIP_woody_AFRI',
  description: '2009_woody',
  fileFormat: 'CSV'
});