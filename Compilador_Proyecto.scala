object HolaMundo{
    //Probando rama alterna en AB_rama
    def Tokens(cadena: String): Boolean = {
        cadena.length() match {
           
            case 3 => {//token para int
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

            case 6 => {//token para double
                if(cadena(0) == ('d') && cadena(1) == ('o' ) && cadena(2) == ('u') && cadena(3) == ('b') && cadena(4) == ('l' ) && cadena(5) == ('e')){
                    true
                }else{
                    false
                }
            }
            
            case 5 => {//token para float
                if(cadena(0) == ('f') && cadena(1) == ('l' ) && cadena(2) == ('o') && cadena(3) == ('a') && cadena(4) == ('t' )){
                    true
                }else{
                    false
                }
            }

            case _ => false
        }
        
    }
    
    def Tabla_Sim(simbolo: Char): String = //tabla de simbolos para caracteres especiales
    {
        simbolo match 
        {
            //caracteres especiales
            case '@' => "Simbolo de referencia"
            case '"' => "Inicio-fin de cadena"
            case '(' => "Principio parentecis"
            case ')' => "Fin parentecis"
            case '%' => "Modular"
            case '/' => "Diagonal"
            case '=' => "Asignador"
            case '+' => "Adicion"
            case '-' => "Sustraccion"
            case '*' => "Multiplicador"
            case '|' => "Simbolo OR"
            case '&' => "Simbolo AND"
            case '!' => "Negacion"
            case ';' => "Fin de linea"
            case _ => "" //default
        }
    }

    def Tabla_Sim_min(sim: Char): Boolean = { //tabla de simbolos para minusculas
        var bandera: Boolean = false
        var caracter: Int = 97
        do{
            if(sim == caracter.toChar)
            {
                bandera = true
            }
            else
            {
                caracter = caracter + 1
            }
        }while(!bandera && caracter < 123)
        bandera
    }

        def Tabla_Sim_MAY(sim: Char): Boolean = { //tabla de simbolos para mayusculas
        var bandera: Boolean = false
        var caracter: Int = 65
        do{
            if(sim == caracter.toChar)
            {
                bandera = true
            }
            else
            {
                caracter = caracter + 1
            }
        }while(!bandera && caracter < 91)
        bandera
    }

    def Analisis_Lexico(linea: String): String =
    {
        var temp: Int = 0
        var pal: String = ""
        var bandera: Boolean = true

        while (linea(temp) == ' ' && temp < linea.length())
        {
            temp += 1
        }

        while (bandera && temp < linea.length())
        {
            if(linea(temp) != ' ' )
            {
                pal += linea(temp)
                temp += 1
            }
            else
            {
                bandera = false
            }
        }

        if(Tokens((pal)))
        {
            if(temp < linea.length())
            {
                s" $pal (Es una palabra reservada)" + Analisis_Lexico(Nueva_Pal(linea,temp))
            }
            else
            {
                s" $pal (Es una palabra reservada)"
            }
            
        }
        else if(pal.length() == 1)
        {
            if(Tabla_Sim(pal(0)) != "")
            {
                if(temp < linea.length())
                {
                    s" $pal " + Tabla_Sim(pal(0)) + Analisis_Lexico(Nueva_Pal(linea,temp))
                }
                else
                {
                    s" $pal " + Tabla_Sim(pal(0))
                }
                
            }
            else
            {
                "Error"
            }
        }
        else
        {
            if(temp < linea.length())
                {
                    s" $pal (Es un identificador)" + Analisis_Lexico(Nueva_Pal(linea,temp))
                }
                else
                {
                    s" $pal (Es un identificador)"
                }
            
        }
    }

    def Nueva_Pal(linea: String, posicion: Int): String = 
    {
        var palabra: String = ""
        var numero: Int = posicion
        while (numero<linea.length())
        {
            palabra += linea(numero)
            numero += 1
        }
        palabra
    }

    def main(args: Array[String]): Unit = 
    {
       do
       {
           println("Introdusca una linea de codigo en busca de Tokens.")
           println(Analisis_Lexico(Console.in.readLine()))
       }while(true)
    }
}