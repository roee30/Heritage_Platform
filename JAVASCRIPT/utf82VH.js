function unique(query,num_seg) {
	var loc=query;
	var total=parseInt(num_seg)+1;
	for (var i=1;i<total;i++){
		var radios=document.getElementsByName(i.toString());
		if((radios!=null)&&(radios.length>=1)){
			for(var j=0;j<radios.length;j++){
				if(radios[j].checked){
					loc=loc.concat(radios[j].value+"|");
				}
			}
		}
		else{
			total++;
		}
	}
	window.location.href=String(loc.substring(0,loc.length-1));
}
function showBox(text, color, obj, e) {
		if(e.layerY>200){
			var node1 = document.getElementById('popBox');
			if((node1!=null)){
				node1.parentNode.removeChild(node1);
			}
			node = document.createElement('div');
			node.style.left = e.layerX + 'px';
			node.style.top = '240pt';
			node.id = 'popBox';
			node.style.backgroundColor= color;
			node.innerHTML = text;
			obj.appendChild(node);
		}
}
function hideBox() {
	node = document.getElementById('popBox');
	node.parentNode.removeChild(node);
}
function normalizeUCharCodeLen(l) {
	switch(l.length) {
		case 0: return '0000';
		case 1: return '000'+l;
		case 2: return '00' +l;
		case 3: return '0'  +l;
		default: return l;
	}
}
function convertDeva(orig) {
	var vowDict={
		"05": "a",
		"06": "aa",
		"07": "i",
		"08": "ii",
		"09": "u",
		"0a": "uu",
		"0b": ".r",
		"60": ".rr",
		"0c": ".l",
		"0f": "e",
		"10": "ai",
		"13": "o",
		"14": "au",
		"4d": ""
	};
	var matDict={
		"3e": "aa",
		"3f": "i",
		"40": "ii",
		"41": "u",
		"42": "uu",
		"43": ".r",
		"44": ".rr",
		"62": ".l",
		"47": "e",
		"48": "ai",
		"4b": "o",
		"4c": "au"
	};
	var consDict={
		"15": "k",
		"16": "kh",
		"17": "g",
		"18": "gh",
		"19": "f",
		"1a": "c",
		"1b": "ch",
		"1c": "j",
		"1d": "jh",
		"1e": "~n",
		"1f": ".t",
		"20": ".th",
		"21": ".d",
		"22": ".dh",
		"23": ".n",
		"24": "t",
		"25": "th",
		"26": "d",
		"27": "dh",
		"28": "n",
		"2a": "p",
		"2b": "ph",
		"2c": "b",
		"2d": "bh",
		"2e": "m",
		"2f": "y",
		"30": "r",
		"32": "l",
		"35": "v",
		"36": "z",
		"37": ".s",
		"38": "s",
		"39": "h"
	};
	var specDict = {
		"01": "~~",
		"02": ".m",
		"03": ".h",
		"3d": "'",
		"64": "|",
		"65": "||"
	}
	var output=''; var wasCons=false;
	var i;
	function tryChar(char,finalizeCons,isCons) {
		if(char!==undefined){
			if(finalizeCons && wasCons){output=output.concat("a"+char);}
			else{output=output.concat(char);}
			wasCons=!!isCons;
			return true;
		}
		return false;
	}
	for(i=0;i<orig.length;i++){
		var charCode = orig.charCodeAt(i)
		if(charCode>=0x0900 && charCode<=0x097F){ // Unicode 12.0 Devanagari range
			var check=normalizeUCharCodeLen(charCode.toString(16)).substring(2);
			tryChar(vowDict[check]) ||
			tryChar(matDict[check]) ||
			tryChar(specDict[check],true) ||
			tryChar(consDict[check],true,true);
		} else {
			tryChar(orig.charAt(i),true);
		}
	}
	if(wasCons){output=output.concat("a");}
	return output;
}
function convertRoma(orig) {
	var romDict={
		"7773": ".rr",
		"0257": "aa",
		"0299": "ii",
		"0363": "uu",
		"7771": ".r",
		"7735": ".l",
		"7749": "f",
		"0241": "~n",
		"7789": ".t",
		"7693": ".d",
		"7751": ".n",
		"7747": ".m",
		"7779": ".s",
		"0347": "z",
		"7717": ".h",
		"7745": "~~"
	};
	var output='';
	var i;
	for(i=0;i<orig.length;i++){
		var check=normalizeUCharCodeLen(orig.charCodeAt(i).toString(10));
		if(romDict[check]!==undefined){
			output=output.concat(romDict[check]);
		}else{
			output=output.concat(orig.charAt(i));
		}
	}
	return output;
}
function convertByTrans(trans_id, focus_id){
	var trans = document.getElementById(trans_id);
	var focus = document.getElementById(focus_id);
	var orig=String(focus.value).trim();
	if(trans.value==='DN'){
		focus.value=convertDeva(orig);
		trans.value='VH';
	}else	if(trans.value==='RN'){
		focus.value=convertRoma(orig);
		trans.value='VH';
	}
}
function convert() {
	var elt1 = document.getElementById('mode_id');
	if((elt1!=null)&&(elt1.length>0)){
		var mode_val=elt1.value;
		var output1=document.getElementById('this_form').action;
		if(mode_val=="g"){
			output1=output1.replace("sktreader","sktgraph");
			output1=output1.replace("sktgraph2","sktgraph");
		}else if((mode_val=="b") || (mode_val=="f") || (mode_val=="l") || (mode_val=="s")){
			if((output1.includes("sktgraph2"))==false){
				output1=output1.replace("sktgraph","sktgraph2");
				output1=output1.replace("sktreader","sktgraph2");
			}
		}else{
			output1=output1.replace("sktgraph2","sktreader");
			output1=output1.replace("sktgraph","sktreader");
		}
		document.getElementById('this_form').action=output1;
	}
	convertByTrans('trans','focus');
}
function convert1() {
	convertByTrans('trans1','focus1');
}
function convert2() {
	var trans = document.getElementById('trans');
	var focus1=document.getElementById('focus1');
	var focus2=document.getElementById('focus2');
	if(trans.value=='DN'){
		focus1.value=convertDeva(String(focus1.value).trim());
		focus2.value=convertDeva(String(focus2.value).trim());
		trans.value='VH';
	}else	if(trans.value=='RN'){
		focus1.value=convertRoma(String(focus1.value).trim());
		focus2.value=convertRoma(String(focus2.value).trim());
		trans.value='VH';
	}
}
