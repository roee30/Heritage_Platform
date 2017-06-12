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
function convert() {
    var elt1 = document.getElementById('mode_id');
    if((elt1!=null)&&(elt1.length>0)){
	var mode_val=elt1.value;
	var output1=document.getElementById('this_form').action;
	if(mode_val=='g'){
	    output1=output1.replace("sktreader","sktgraph");
	}
	else{
	    output1=output1.replace("sktgraph","sktreader");
	}
	document.getElementById('this_form').action=output1;
    }
    var elt = document.getElementById('trans');
    var trans=elt.value;
	if(trans=='DN'){
	    var inHex=["05","06","07","08","09","0a","0b","60","0c","0f","10","13","14","02","01","03","3d","4d"];
	    var outVH=["a","aa","i","ii","u","uu",".r",".rr",".l","e","ai","o","au",".m","~l",".h","'",""];
	    var matIn=["3e","3f","40","41","42","43","44","62","47","48","4b","4c"];
	    var consIn=["15","16","17", "18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","2a","2b","2c","2d","2e","2f","30","32","35","36","37","38","39","00"];
	    var orig=String(document.getElementById('focus').value).trim();
	    var output='';var wasCons=false;var output1='';
	    for(i=0;i<orig.length;i++){
		var origC=orig.charAt(i);
		var l=orig.charCodeAt(i).toString(16);
		var lenL=l.length;
		if(lenL==0) l='0000';
		if(lenL==1) l='000' +l;
		if(lenL==2) l='00' +l;
		if(lenL==3) l='0' +l;
		var check=l.substring(2);
		var init=l.substring(0,2);
		if(init!='09'){check='00';}
		var consOut=["k","kh","g","gh","f","c","ch","j","jh","~n",".t",".th",".d",".dh",".n","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","z",".s","s","h",origC+""];
		for(j=0;j<inHex.length;j++){
		    if(check==inHex[j]){
			if((check=="03")||(check=="01")||(check=="02")||(check=="3d")){
			    if(wasCons){output=output.concat("a"+outVH[j]);}
			    else{output=output.concat(outVH[j]);}
			}
			else{output=output.concat(outVH[j]);}
			wasCons=false;
		    }			
		}
		for(j=0;j<consIn.length;j++){
		    if(check==consIn[j]){
			if(wasCons){output=output.concat("a"+consOut[j]);}
			else{output=output.concat(consOut[j]);}
			if(check!='00'){	wasCons=true;}
			else{wasCons=false;}
			if(i==orig.length-1){output=output.concat("a");}
		    }
		}
		for(j=0;j<matIn.length;j++){
		    if(check==matIn[j]){output=output.concat(outVH[j+1]);wasCons=false;}
		}
	    }
	    document.getElementById('focus').value=output;
	    elt.value='VH';
	}
 	if(trans=='RN'){
	    var inRom= ["7773","0257","0299","0363","7771","7735","7749","0241","7789","7693","7751","7747","7779","0347","7717"];
	    var outVH= [".rr","aa","ii","uu",".r",".l","f","~n",".t",".d",".n",".m",".s","z",".h"];
	    var orig=String(document.getElementById('focus').value).trim();
	    var output='';
	    for(i=0;i<orig.length;i++){
		var origC=orig.charAt(i);
		var l=orig.charCodeAt(i);
		var lenL=l.length;
		if(lenL==0) l='0000';
		if(lenL==1) l='000' +l;
		if(lenL==2) l='00' +l;
		if(lenL==3) l='0' +l;
		var check=l;
		var isC=false;
		for(j=0;j<inRom.length;j++){
		    if(check==inRom[j]){
			output=output.concat(outVH[j]);isC=true;
		    }
		}
		if(!isC){
		    output=output.concat(origC);
		}
	    }
 	    document.getElementById('focus').value=output;
 	    elt.value='VH';
	}
}
function convert1() {
	var elt = document.getElementById('trans1');
	var trans=elt.value;
	if(trans=='DN'){
		var inHex=["05","06","07","08","09","0a","0b","60","0c","0f","10","13","14","02","01","03","3d","4d"];
		var outVH=["a","aa","i","ii","u","uu",".r",".rr",".l","e","ai","o","au",".m","~l",".h","'",""];
		var matIn=["3e","3f","40","41","42","43","44","62","47","48","4b","4c"];
		var consIn=["15","16","17", "18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","2a","2b","2c","2d","2e","2f","30","32","35","36","37","38","39","00"];
		var orig=String(document.getElementById('focus1').value).trim();
		var output='';var wasCons=false;var output1='';
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i).toString(16);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l.substring(2);
			var init=l.substring(0,2);
			if(init!='09'){check='00';}
			var consOut=["k","kh","g","gh","f","c","ch","j","jh","~n",".t",".th",".d",".dh",".n","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","z",".s","s","h",origC+""];
			for(j=0;j<inHex.length;j++){
				if(check==inHex[j]){
					if((check=="03")||(check=="01")||(check=="02")||(check=="3d")){
						if(wasCons){output=output.concat("a"+outVH[j]);}
						else{output=output.concat(outVH[j]);}
					}
					else{output=output.concat(outVH[j]);}
					wasCons=false;
				}			
			}
			for(j=0;j<consIn.length;j++){
				if(check==consIn[j]){
					if(wasCons){output=output.concat("a"+consOut[j]);}
					else{output=output.concat(consOut[j]);}
					if(check!='00'){	wasCons=true;}
					else{wasCons=false;}
					if(i==orig.length-1){output=output.concat("a");}
				}
			}
			for(j=0;j<matIn.length;j++){
				if(check==matIn[j]){output=output.concat(outVH[j+1]);wasCons=false;}
			}
		}
		document.getElementById('focus1').value=output;
		elt.value='VH';
	}
	if(trans=='RN'){
		var inRom= ["7773","0257","0299","0363","7771","7735","7749","0241","7789","7693","7751","7747","7779","0347","7717"];
	       var outVH= [".rr","aa","ii","uu",".r",".l","f","~n",".t",".d",".n",".m",".s","z",".h"];
		var orig=String(document.getElementById('focus1').value).trim();
		var output='';
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l;
			var isC=false;
			for(j=0;j<inRom.length;j++){
				if(check==inRom[j]){
					output=output.concat(outVH[j]);isC=true;
				}
			}
			if(!isC){
				output=output.concat(origC);
			}
		}
 		document.getElementById('focus1').value=output;
 		elt.value='VH';
	}
}
function convert2() {
	var elt = document.getElementById('trans');
	var trans=elt.value;
	if(trans=='DN'){
		var inHex=["05","06","07","08","09","0a","0b","60","0c","0f","10","13","14","02","01","03","3d","4d"];
		var outVH=["a","aa","i","ii","u","uu",".r",".rr",".l","e","ai","o","au",".m","~l",".h","'",""];
		var matIn=["3e","3f","40","41","42","43","44","62","47","48","4b","4c"];
		var consIn=["15","16","17", "18","19","1a","1b","1c","1d","1e","1f","20","21","22","23","24","25","26","27","28","2a","2b","2c","2d","2e","2f","30","32","35","36","37","38","39","00"];
		var orig=String(document.getElementById('focus1').value).trim();
		var output='';var wasCons=false;var output1='';
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i).toString(16);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l.substring(2);
			var init=l.substring(0,2);
			if(init!='09'){check='00';}
			var consOut=["k","kh","g","gh","f","c","ch","j","jh","~n",".t",".th",".d",".dh",".n","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","z",".s","s","h",origC+""];
			for(j=0;j<inHex.length;j++){
				if(check==inHex[j]){
					if((check=="03")||(check=="01")||(check=="02")||(check=="3d")){
						if(wasCons){output=output.concat("a"+outVH[j]);}
						else{output=output.concat(outVH[j]);}
					}
					else{output=output.concat(outVH[j]);}
					wasCons=false;
				}			
			}
			for(j=0;j<consIn.length;j++){
				if(check==consIn[j]){
					if(wasCons){output=output.concat("a"+consOut[j]);}
					else{output=output.concat(consOut[j]);}
					if(check!='00'){	wasCons=true;}
					else{wasCons=false;}
					if(i==orig.length-1){output=output.concat("a");}
				}
			}
			for(j=0;j<matIn.length;j++){
				if(check==matIn[j]){output=output.concat(outVH[j+1]);wasCons=false;}
			}
		}
		document.getElementById('focus1').value=output;
		
		orig=String(document.getElementById('focus2').value).trim();
		output='';wasCons=false;
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i).toString(16);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l.substring(2);
			var init=l.substring(0,2);
			if(init!='09'){check='00';}
			var consOut=["k","kh","g","gh","f","c","ch","j","jh","~n",".t",".th",".d",".dh",".n","t","th","d","dh","n","p","ph","b","bh","m","y","r","l","v","z",".s","s","h",origC+""];
			for(j=0;j<inHex.length;j++){
				if(check==inHex[j]){
					if((check=="03")||(check=="01")||(check=="02")||(check=="3d")){
						if(wasCons){output=output.concat("a"+outVH[j]);}
						else{output=output.concat(outVH[j]);}
					}
					else{output=output.concat(outVH[j]);}
					wasCons=false;
				}			
			}
			for(j=0;j<consIn.length;j++){
				if(check==consIn[j]){
					if(wasCons){output=output.concat("a"+consOut[j]);}
					else{output=output.concat(consOut[j]);}
					if(check!='00'){	wasCons=true;}
					else{wasCons=false;}
					if(i==orig.length-1){output=output.concat("a");}
				}
			}
			for(j=0;j<matIn.length;j++){
				if(check==matIn[j]){output=output.concat(outVH[j+1]);wasCons=false;}
			}
		}
		document.getElementById('focus2').value=output;
		elt.value='VH';
	}
	if(trans=='RN'){
		var inRom= ["7773","0257","0299","0363","7771","7735","7749","0241","7789","7693","7751","7747","7779","0347","7717"];
	       var outVH= [".rr","aa","ii","uu",".r",".l","f","~n",".t",".d",".n",".m",".s","z",".h"];
		var orig=String(document.getElementById('focus1').value).trim();
		var output='';
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l;
			var isC=false;
			for(j=0;j<inRom.length;j++){
				if(check==inRom[j]){
					output=output.concat(outVH[j]);isC=true;
				}
			}
			if(!isC){
				output=output.concat(origC);
			}
		}
 		document.getElementById('focus1').value=output;
		orig=String(document.getElementById('focus2').value).trim();
		output='';
		for(i=0;i<orig.length;i++){
			var origC=orig.charAt(i);
			var l=orig.charCodeAt(i);
			var lenL=l.length;
			if(lenL==0) l='0000';
			if(lenL==1) l='000' +l;
			if(lenL==2) l='00' +l;
			if(lenL==3) l='0' +l;
			var check=l;
			var isC=false;
			for(j=0;j<inRom.length;j++){
				if(check==inRom[j]){
					output=output.concat(outVH[j]);isC=true;
				}
			}
			if(!isC){
				output=output.concat(origC);
			}
		}
		document.getElementById('focus2').value=output;
		elt.value='VH';
	}
}
