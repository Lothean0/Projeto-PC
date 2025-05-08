import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import processing.core.PApplet;
import java.util.logging.Logger;
import java.util.logging.Level;

import static java.lang.Thread.sleep;

public class Client extends PApplet {
    String currentScene = "Menu";
    String username = "";
    String password = "";
    boolean typingUsername = true;
    boolean typingPassword = false;
    boolean ignoreFirstClick = false;
    Socket socket;
    PrintWriter out;
    BufferedReader in;
    String Lvl = "";

    public void settings(){
        size(800, 800);
    }

    public void setup() {
        smooth();
        textSize(16);
        textAlign(CENTER, CENTER);
        connectToServer();
    }

    public void draw() {
        background(0);

        switch (currentScene) {
            case "Login":
                drawLogin();
                break;
            case "CreateAccount":
                drawCreateAccount();
                break;
            case "MatchPage":
                drawMatchPage();
                break;
            default:
                drawMenu();
                break;
        }
    }

    private void drawMenu(){
        fill(255);
        textSize(32);
        text("Menu", (float) width / 2, (float) height / 2 - 70);
        textSize(16);

        // Botão de Login
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 - 20, 100, 30, 10);
        fill(0);
        text("Login", (float) width / 2, (float) height / 2 - 6);

        // Botão de Criar Conta
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Criar Conta", (float) width / 2, (float) height / 2 + 55);
    }

    private void drawLogin(){
        fill(255);
        textSize(32);
        text("Login", (float) width / 2, (float) height / 2 - 130);
        textSize(16);

        // Caixa de username
        fill(typingUsername ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 70, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        text(username, (float) width / 2 - 95, (float) height / 2 - 55);
        textAlign(CENTER, CENTER);

        // Label para username
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Username:", (float) width / 2 - 100, (float) height / 2 - 75);
        textAlign(CENTER, CENTER);

        // Caixa de password
        fill(typingPassword ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 10, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        // Mostrar asteriscos para a senha
        StringBuilder maskedPassword = new StringBuilder();
        for (int i = 0; i < password.length(); i++) {
            maskedPassword.append("*");
        }
        text(maskedPassword.toString(), (float) width / 2 - 95, (float) height / 2 + 5);
        textAlign(CENTER, CENTER);

        // Label para password
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Password:", (float) width / 2 - 100, (float) height / 2 - 15);
        textAlign(CENTER, CENTER);

        // Botão de login
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Login", (float) width / 2, (float) height / 2 + 55);

        // Botão de voltar
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 80, 100, 30, 10);
        fill(0);
        text("Voltar", (float) width / 2, (float) height / 2 + 95);


    }

    private void drawCreateAccount(){
        fill(255);
        textSize(32);
        text("Criar Conta", (float) width / 2, (float) height / 2 - 130);
        textSize(16);

        // Caixa de username
        fill(typingUsername ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 -  70, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        text(username, (float) width / 2 - 95, (float) height / 2 - 55);
        textAlign(CENTER, CENTER);

        // Label para username
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Username:", (float) width / 2 - 100, (float) height / 2 - 75);
        textAlign(CENTER, CENTER);

        // Caixa de password
        fill(typingPassword ? 200 : 150);
        rect((float) width / 2 - 100, (float) height / 2 - 10, 200, 30, 10);
        fill(0);
        textAlign(LEFT, CENTER);
        // Mostrar asteriscos para a senha
        StringBuilder maskedPassword = new StringBuilder();
        for (int i = 0; i < password.length(); i++) {
            maskedPassword.append("*");
        }
        text(maskedPassword.toString(), (float) width / 2 - 95, (float) height / 2 + 5);
        textAlign(CENTER, CENTER);

        // Label para password
        fill(255);
        textAlign(LEFT, BOTTOM);
        text("Password:", (float) width / 2 - 100, (float) height / 2 - 15);
        textAlign(CENTER, CENTER);

        // Botão de criar conta
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 40, 100, 30, 10);
        fill(0);
        text("Criar Conta", (float) width / 2, (float) height / 2 + 55);

        // Botão de voltar
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 80, 100, 30, 10);
        fill(0);
        text("Voltar", (float) width / 2, (float) height / 2 + 95);

    }

    private void drawMatchPage(){
        fill(0);
        textSize(32);
        fill(255);
        text("User: " + username, (float) width / 2, (float) height / 2 - 100);

        textSize(16);
        text("Level: " + Lvl , (float) width / 2, (float) height / 2 - 60);
        text("Vitorias: 0", (float) width / 2, (float) height / 2 - 30);
        text("Derrotas: 0", (float) width / 2, (float) height / 2);
        text("Streak de vitorias : 0", (float) width / 2, (float) height / 2 + 30);

        // Botão de Logout
        fill(200, 100, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 95, 100, 30, 10);
        fill(0);
        text("Logout", (float) width / 2, (float) height / 2 + 110);

        // Botão de Match
        fill(100, 200, 100);
        rect((float) width / 2 - 50, (float) height / 2 + 55, 100, 30, 10);
        fill(0);
        text("Match", (float) width / 2, (float) height / 2 + 70);

    }

    public void mousePressed() {
        String response = "";
        if (currentScene.equals("Menu")) {
            // Verifica clique no botão "Login"
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 - 20 && mouseY < height / 2 + 10) {
                currentScene = "Login";
                typingUsername = false;
                typingPassword = false;
                ignoreFirstClick = true;
            }
            // Verifica clique no botão "Criar Conta"
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                currentScene = "CreateAccount";
                typingUsername = false;
                typingPassword = false;
                ignoreFirstClick = true;
            }
        }

        if (currentScene.equals("Login")) {
            if (ignoreFirstClick) {
                ignoreFirstClick = false; // Ignora o clique inicial
                return;
            }
            typingUsername = false;
            typingPassword = false;
            // Botão voltar
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 80 && mouseY < height / 2 + 110) {
                currentScene = "Menu";
                username = "";
                password = "";
            }
            // Botão login
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                // Lógica de login aqui
                if (!username.isEmpty() && !password.isEmpty()){
                    out.println("/l " + username + " " + password);
                    out.flush();
                    try {
                        response = in.readLine();

                    if (response.equalsIgnoreCase("Login successful")) {
                        System.out.println(response);
                        out.println("/Lv");
                        out.flush();
                        Lvl = in.readLine();
                        currentScene = "MatchPage";
                    } else {
                        System.out.println(response);
                    }
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, "Erro ao ler do servidor", e);
                    }
                }
            }
            // Caixa de username
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 120 &&
                    mouseY > height / 2 - 60 && mouseY < height / 2 - 30) {
                typingUsername = true;
                println("Caixa de username ativada");
            }
            // Caixa de password
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 120 &&
                    mouseY > height / 2 - 10 && mouseY < height / 2 + 20) {
                typingPassword = true;
                println("Caixa de password ativada");
            }
        }

        if (currentScene.equals("CreateAccount")) {
            if (ignoreFirstClick) {
                ignoreFirstClick = false; // Ignora o clique inicial
                return;
            }
            typingUsername = false;
            typingPassword = false;
            // Botão voltar
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 80 && mouseY < height / 2 + 110) {
                currentScene = "Menu";
                username = "";
                password = "";
            }
            // Botão criar conta
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                if (!username.isEmpty() && !password.isEmpty()){
                    out.println("/cr " + username + " " + password);
                    out.flush();
                    try {
                        response = in.readLine();
                    } catch (IOException e) {
                        logger.log(Level.SEVERE, "Erro ao ler do servidor", e);
                    }
                    if (response.equals("Account created successfully")) {
                        System.out.println(response);
                        currentScene = "Menu";
                        username = "";
                        password = "";
                    } else {
                        System.out.println(response);
                        username = "";
                        password = "";
                    }

                }
            }
            // Caixa de username
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 100 &&
                    mouseY > height / 2 - 60 && mouseY < height / 2 - 30) {
                typingUsername = true;
                println("Caixa de username ativada");
            }
            // Caixa de password
            else if (mouseX > width / 2 - 100 && mouseX < width / 2 + 100 &&
                    mouseY > height / 2 - 10 && mouseY < height / 2 + 20) {
                typingPassword = true;
                println("Caixa de password ativada");
            }
        }

        if (currentScene.equals("MatchPage")) {
            // Botão de Logout
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 - 20 && mouseY < height / 2 + 10) {
                currentScene = "Menu";
                username = "";
                password = "";
            }
            // Botão de Match
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                out.println("/f");
                out.flush();
            }
        }
    }

    public void keyPressed() {
        if (currentScene.equals("Login") || currentScene.equals("CreateAccount")) {
            if (typingUsername) {
                if (key == BACKSPACE && !username.isEmpty()) {
                    username = username.substring(0, username.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    username += key;
                } else if (key == TAB) {
                    typingUsername = false;
                    typingPassword = true;
                }
                println("Username: " + username);
            } else {
                if (key == BACKSPACE && !password.isEmpty()) {
                    password = password.substring(0, password.length() - 1);
                } else if (key != BACKSPACE && key != ENTER && key != TAB && key != CODED) {
                    password += key;
                } else if (key == TAB) {
                    typingPassword = false;
                    typingUsername = true;
                }
                println("Password: " + password);
            }
        }

        if (currentScene.equals("MatchPage")) {
            // Botão de Logout
            if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 - 20 && mouseY < height / 2 + 10) {
                currentScene = "Menu";
                username = "";
                password = "";
            }
            // Botão de Match
            else if (mouseX > width / 2 - 50 && mouseX < width / 2 + 50 &&
                    mouseY > height / 2 + 40 && mouseY < height / 2 + 70) {
                out.println("/f");
                out.flush();
            }
        }

    }

    private static final Logger logger = Logger.getLogger(Client.class.getName());

    void connectToServer() {
        try {
            socket = new Socket("127.0.0.1", Integer.parseInt("8000"));
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            out = new PrintWriter(socket.getOutputStream(), true);

            // Thread para ler do servidor
            /*
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
            */

                // Fechar recursos
        } catch (IOException e) {
                logger.log(Level.SEVERE, "Erro na comunicação com o servidor", e);
                }
        }

    public static void main(String[] args) {
        PApplet.main("Client");
    }
}