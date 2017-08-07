/**************************************************************************/
/*                                                                        */
/*                     The Sanskrit Heritage Platform                     */
/*                                                                        */
/*                              Idir Lankri                               */
/*                                                                        */
/* ©2017 Institut National de Recherche en Informatique et en Automatique */
/**************************************************************************/

function hideShowElement(id) {
    var elt = document.getElementById(id);
    if (elt.style.display === 'block') {
	elt.style.display = 'none';
    } else {
	elt.style.display = 'block';
    }
}
