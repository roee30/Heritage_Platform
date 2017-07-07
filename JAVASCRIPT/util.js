function hideShowElement(id) {
    var elt = document.getElementById(id);
    if (elt.style.display === 'block') {
	elt.style.display = 'none';
    } else {
	elt.style.display = 'block';
    }
}
