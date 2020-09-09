class Feedback{
    constructor(){
        var edit = document.querySelector(".project.edit");
        if(edit){
            var template = edit.querySelector(".attachment").cloneNode(true);
            edit.querySelector(".button.add").addEventListener("click", (e)=>{
                var copy = template.cloneNode(true);
                copy.querySelector(".button.delete").addEventListener("click", (e)=>copy.remove());
                edit.querySelector(".attachments ul").appendChild(copy);
            });
            [].forEach.call(edit.querySelectorAll(".attachment"), (a)=>{
                a.querySelector(".button.delete").addEventListener("click", (e)=>a.remove());
            });
        }
    }
};

var feedback = new Feedback();
