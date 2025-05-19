let raw_input = [];
let full_nametable= [];
let final_nametable = [];
let id_table = [];
let id_byte, amt_id, line_num, amt_line, columnwise

const nt = document.getElementById("nt_input_form");
const toggle_disp = document.querySelector("#toggle");
const og_disp = document.querySelector("#full_nametable_disp")

function parseNametable(str) {
    let newStr = str.replace(/[\[\]\s\,\$\.'"\\n]+/g, "");
    const result = [];
    for (let i = 0; i < newStr.length; i += 2) {
        let entry = newStr.substring(i, i + 2);
        let parse = parseInt(entry, 16);
        result.push(parse);
    }

    if (result.length < 960) {
        let remainder = 960 - result.length;
        for (let i=0; i<remainder; i++) {
            result.push(0);
        } 
    }
    return result;
}

function createDisplayString(nametable){
    let dispNTstr = '';
    for(i=0;i<960;i+=32){
        for(j=0;j<32;j++){
            dispNTstr = dispNTstr + nametable[(j+i)].toString(16).padStart(2,0).toUpperCase() + ',';
        }
        dispNTstr = dispNTstr + '<br>';
    }
    return dispNTstr;
}

nt.addEventListener('submit', function(event) {
    event.preventDefault(); // Prevent the default form submission

    const formData = new FormData(nt);
    for (const [key, value] of formData.entries()) {
        console.log(key, value);
    }
    raw_input = formData.getAll("nt_input");
    console.log("raw" + raw_input);
    full_nametable = parseNametable(JSON.stringify(raw_input));
    console.log(full_nametable);
    
    const dispNT = document.getElementById("full_nametable_disp");
    const display = createDisplayString(full_nametable);
    dispNT.innerHTML = "Originating Nametable<br>" + display;
});

og_disp.style.visibilty = "hidden";
toggle_disp.addEventListener('change', () => {
    if (toggle_disp.checked) {
        og_disp.style.visibility = "visible";
    } else {
        og_disp.style.visibility = "hidden";
    }
})


function findCanvasID(nametable){       //determine most abundant ID in nametable
    
}

//return to a id_table array
function gatherAllOtherIDs(nametable){  //sort descending by abundance at return

}

//return array object. Can call with final_nametable.push(compressID(full_nametable, id_table[0])).
//Call this in for loop and that should be it.
function compressID(nametable, id){

}

function convertToUint8(nametable){

}

//Time for compresssion code. What are my steps?
//-determine most abundant id and make it the canvas
//-toggle for row-wise or column-wise
//-collect remaining IDs into an array, sort descending by abundancy
//-branch for row/column-wise (following steps based on column-wise)

//-add 128 to first ID, then add to nametable with toString(16) 
//-push first ID to final_nametable.
//-find first column in which it appears, from left
//-begin add to empty string with toString(2).padStart(5, '0')
//-count how many instances in that column
//-append with toString(2).padStart(5, '0')
//-for each instance within that column, append roww position with toString(2).padStart(5, 0).
//-parse full string 8 bits at a time and push to array with toString(16).padStart(2, 0)
//-repeat for each ID in the Id array
//-?Consider converting to Uint8 array
//-calculate total bytes
//-choose best way to display array and size back to the user (copy and pastable for use in code)