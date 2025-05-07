import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import processing.core.PApplet;
import java.util.logging.Logger;
import java.util.logging.Level;

public class Client extends PApplet {

    public void settings(){
        size(800, 800);
    }

    public void setup(){

    }

    public void draw(){
        ellipse(300, 300, 60, 60);
    }

    private static final Logger logger = Logger.getLogger(Client.class.getName());

    public static void main(String[] args) {
        PApplet.main("Client");

        try {
            BufferedReader cli = new BufferedReader(new InputStreamReader(System.in));
            Socket socket = new Socket("127.0.0.1", Integer.parseInt("8000"));
            BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            PrintWriter out = new PrintWriter(socket.getOutputStream());

            // Thread para ler do servidor
            Thread readerThread = new Thread(() -> {
                while (true) {
                    try {
                        System.out.println(in.readLine());
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, "Erro ao ler do servidor", e);
                        break;
                    }
                }
            });
            readerThread.start();

            // Entrada do utilizador
            String line;
            while ((line = cli.readLine()) != null) {
                if (line.equals("exit")) {
                    out.println(line);
                    out.flush();
                    break;
                }
                out.println(line);
                out.flush();
            }

            // Fechar recursos
            socket.close();
            cli.close();
        } catch (IOException e) {
            logger.log(Level.SEVERE, "Erro na comunicação com o servidor", e);
        }
    }
}