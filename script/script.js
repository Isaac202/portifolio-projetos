
const hid =  document.getElementById('hidden');
const trans = document.getElementById('transformer')

alterar = () => {
        hid.style.display = " block"
};

sair = () =>{
   hid.style.display = "none" 
};

trans.addEventListener('mouseover', alterar)
trans.addEventListener('mouseout', sair)