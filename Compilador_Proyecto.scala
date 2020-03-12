object HolaMundo{
    //Probando rama alterna en AB_rama
    def Tokens(cadena: String): Boolean = {
        cadena.length() match {//token para int
            case 3 => {
                if(cadena(0) == ('i') && cadena(1) == ('n') && cadena(2) == ('t')){
                    true
                }else{
                    false
                }
            }
            case 4 => {// token para char
                if(cadena(0) == ('c') && cadena(1) == ('h' ) && cadena(2) == ('a') && cadena(3) == ('r')){
                    true
                }else{
                    false
                }
            }
            case _ => false
        }
        
    }
    
    def Tabla_Sim(simbolo: Char): Unit = {
        println(
            simbolo match {
                //caracteres especiales
                case '@' => "Simbolo de referencia"
                case '"' => "Inicio-fin de cadena"
                case '(' => "Principio parentecis"
                case ')' => "Fin parentecis"
                case '%' => "Modular"
                case '/' => "Diagonal"
                case '=' => "Asignador"
                case '*' => "Multiplicador"
                case '|' => "Simbolo OR"
                case '&' => "Simbolo AND"
                case '!' => "Negacion"
                //Minusculas a-z
                case 'a' => "Letra minuscula a"
                case 'b' => "Letra minuscula b"
                case 'c' => "Letra minuscula c"
                case 'd' => "Letra minuscula d"
                case 'e' => "Letra minuscula e"
                case 'f' => "Letra minuscula f"
                case 'g' => "Letra minuscula g"
                case 'h' => "Letra minuscula h"
                case 'i' => "Letra minuscula i"
                case 'j' => "Letra minuscula j"
                case 'k' => "Letra minuscula k"
                case 'l' => "Letra minuscula l"
                case 'm' => "Letra minuscula m"
                case 'n' => "Letra minuscula n"
                case 'ñ' => "Letra minuscula ñ"
                case 'o' => "Letra minuscula o"
                case 'p' => "Letra minuscula p"
                case 'q' => "Letra minuscula q"
                case 'r' => "Letra minuscula r"
                case 's' => "Letra minuscula s"
                case 't' => "Letra minuscula t"
                case 'u' => "Letra minuscula u"
                case 'v' => "Letra minuscula v"
                case 'w' => "Letra minuscula w"
                case 'x' => "Letra minuscula x"
                case 'y' => "Letra minuscula y"
                case 'z' => "Letra minuscula z"
                //mayusculas A-Z
                case 'A' => "Letra mayuscula A"
                case 'B' => "Letra mayuscula B"
                case 'C' => "Letra mayuscula C"
                case 'D' => "Letra mayuscula D"
                case 'E' => "Letra mayuscula E"
                case 'F' => "Letra mayuscula F"
                case 'G' => "Letra mayuscula G"
                case 'H' => "Letra mayuscula H"
                case 'I' => "Letra mayuscula I"
                case 'J' => "Letra mayuscula J"
                case 'K' => "Letra mayuscula K"
                case 'L' => "Letra mayuscula L"
                case 'M' => "Letra mayuscula M"
                case 'N' => "Letra mayuscula N"
                case 'Ñ' => "Letra mayuscula Ñ"
                case 'O' => "Letra mayuscula O"
                case 'P' => "Letra mayuscula P"
                case 'Q' => "Letra mayuscula Q"
                case 'R' => "Letra mayuscula R"
                case 'S' => "Letra mayuscula S"
                case 'T' => "Letra mayuscula T"
                case 'U' => "Letra mayuscula U"
                case 'V' => "Letra mayuscula V"
                case 'W' => "Letra mayuscula W"
                case 'X' => "Letra mayuscula X"
                case 'Y' => "Letra mayuscula Y"
                case 'Z' => "Letra mayuscula Z"
                //Numeros 0-9
                case '0' => "Numero 0"
                case '1' => "Numero 1"
                case '2' => "Numero 2"
                case '3' => "Numero 3"
                case '4' => "Numero 4"
                case '5' => "Numero 5"
                case '6' => "Numero 6"
                case '7' => "Numero 7"
                case '8' => "Numero 8"
                case '9' => "Numero 9"
                case _ => "No se encontro el simbolo" //default
            }
        )
    }

    def main(args: Array[String]): Unit = {
        do{
            println("Ingrese un simbolo para buscar en tabla de simbolos.")
            Tabla_Sim(Console.in.readLine()(0))
            println("Ingrese una palabra para verificarla como token.")
            println(
            if(Tokens(Console.in.readLine())){
                "Si existe entre los tokens"
            }else{
                "No existe entre los tokens"
            }
        )
        }while(true)
    }
}




















/*
        println("Tabla de simbolos")
        val k="Interpolar cadena"
        println(s"hola mundo $k")//Interpolar cadena (concatenacion)
        */
//Componenetes lexico, Tipo de alfabeto, expresion regular(lenguajes), tabla de simbolos
/*public miclase{
    protected mi clase x;
    private int y;
    public string z;
}*/
/*
Tabla de simbolos
Nombre  Tipo    ambito    Visibilidad Tamaño  Posicion    rul
miclase class   global      publico     3       -          clase
x       miclase miclase     protected   1       0          atrivuto
y       int     miclase     private     1       1          atrivuto
z       string  miclase     public      1       2          atrivuto
*/
//cd para moverse entre directorios
/*Ejecutar: Scala "Nombre".scala
metodo def
ejemplo:
def suma(a: Int, b: Int): Int = {
    a+b
}

utilizando:
suma(5+5)
res3: Int = 10
*/