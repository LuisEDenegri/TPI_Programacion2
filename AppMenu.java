package main;

import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

import service.DispositivoIotService;
import model.DispositivoIoT;

public class AppMenu {

    private Scanner scanner = new Scanner(System.in);
    private DispositivoIotService service = new DispositivoIotService();

    public void displayMenu() {

        int opcion = -1;

        do {
            System.out.println("\n--- Menú DispositivoIoT y ConfiguraciónRed ---");
            System.out.println("1. Crear DispositivoIoT (transaccional)");
            System.out.println("2. Leer DispositivoIoT por ID");
            System.out.println("3. Listar todos los DispositivosIoT (incluye eliminados lógicamente)");
            System.out.println("4. Actualizar DispositivoIoT");
            System.out.println("5. Eliminar (Baja Lógica) DispositivoIoT");
            System.out.println("6. Buscar por Serial (campo relevante)");
            System.out.println("0. Salir");
            System.out.print("Seleccione una opción: ");

            try {
                opcion = Integer.parseInt(scanner.nextLine());
            } catch (NumberFormatException e) {
                System.out.println("⚠ Error: Debe ingresar un número válido.");
                continue;
            }

            switch (opcion) {
                case 1: crear(); break;
                case 2: leerPorId(); break;
                case 3: listar(); break;
                case 4: actualizar(); break;
                case 5: eliminar(); break;
                case 6: buscarPorSerial(); break;
                case 0: System.out.println("Saliendo..."); break;
                default: System.out.println("⚠ Opción inválida. Intente nuevamente.");
            }

        } while (opcion != 0);
    }

    private void crear() {
        try {
            System.out.print("Serial (único): ");
            String serial = scanner.nextLine().trim();

            System.out.print("Modelo: ");
            String modelo = scanner.nextLine().trim();

            System.out.print("Ubicación: ");
            String ubicacion = scanner.nextLine().trim();

            System.out.print("Firmware: ");
            String firmware = scanner.nextLine().trim();

            DispositivoIoT dispositivo = new DispositivoIoT();
            dispositivo.setSerial(serial);
            dispositivo.setModelo(modelo);
            dispositivo.setUbicacion(ubicacion);
            dispositivo.setFirmwareVersion(firmware);
            dispositivo.setEliminado(false);

            service.create(dispositivo);

            System.out.println("✔ Dispositivo creado correctamente.");

        } catch (Exception e) {
            if (e.getMessage().contains("Duplicate entry")) {
                System.out.println("❌ Error: Serial ya existe (violación de unicidad).");
            } else {
                System.out.println("❌ Error al crear: " + e.getMessage());
            }
        }
    }

    private void leerPorId() {
        try {
            System.out.print("Ingrese ID: ");
            long id = Long.parseLong(scanner.nextLine());

            DispositivoIoT disp = service.getById(id);

            if (disp == null) {
                System.out.println("❌ No existe un dispositivo con ese ID.");
            } else {
                System.out.println("✔ Encontrado: " + disp);
            }

        } catch (NumberFormatException e) {
            System.out.println("⚠ Error: Debe ingresar un número válido para el ID.");
        } catch (Exception e) {
            System.out.println("❌ Error: " + e.getMessage());
        }
    }

    private void listar() {
        try {
            List<DispositivoIoT> lista = service.getAll();

            if (lista.isEmpty()) {
                System.out.println("⚠ La base de datos está vacía.");
            } else {
                System.out.println("--- Lista de Dispositivos ---");
                lista.forEach(System.out::println);
            }

        } catch (Exception e) {
            System.out.println("❌ Error al listar: " + e.getMessage());
        }
    }

    private void actualizar() {
        try {
            System.out.print("Ingrese ID del dispositivo a actualizar: ");
            long id = Long.parseLong(scanner.nextLine());

            DispositivoIoT existente = service.getById(id);

            if (existente == null) {
                System.out.println("❌ No existe un dispositivo con ese ID.");
                return;
            }

            System.out.print("Nuevo modelo (" + existente.getModelo() + "): ");
            String modelo = scanner.nextLine().trim();

            System.out.print("Nueva ubicación (" + existente.getUbicacion() + "): ");
            String ubicacion = scanner.nextLine().trim();

            System.out.print("Nuevo firmware (" + existente.getFirmwareVersion() + "): ");
            String firmware = scanner.nextLine().trim();

            if (!modelo.isEmpty()) existente.setModelo(modelo);
            if (!ubicacion.isEmpty()) existente.setUbicacion(ubicacion);
            if (!firmware.isEmpty()) existente.setFirmwareVersion(firmware);

            service.update(existente);

            System.out.println("✔ Dispositivo actualizado correctamente.");

        } catch (NumberFormatException e) {
            System.out.println("⚠ Error: Debe ingresar un número válido.");
        } catch (Exception e) {
            System.out.println("❌ Error: " + e.getMessage());
        }
    }

    private void eliminar() {
        try {
            System.out.print("Ingrese ID a eliminar (baja lógica): ");
            long id = Long.parseLong(scanner.nextLine());

            boolean ok = service.delete(id);

            if (ok) {
                System.out.println("✔ Baja lógica realizada.");
            } else {
                System.out.println("❌ No existe el dispositivo o ya estaba eliminado.");
            }

        } catch (NumberFormatException e) {
            System.out.println("⚠ Error: debe ingresar un número válido.");
        } catch (Exception e) {
            System.out.println("❌ Error: " + e.getMessage());
        }
    }

    public void buscarPorSerial() {

        System.out.print("Ingrese el serial a buscar: ");
        String serial = scanner.nextLine().trim();

        try {
            DispositivoIoT disp = service.getBySerial(serial);

            if (disp == null) {
                System.out.println("❌ No existe un dispositivo con ese serial.");
            } else {
                System.out.println("✔ Encontrado: " + disp);
            }

        } catch (Exception e) {
            System.out.println("❌ Error: " + e.getMessage());
        }
    }
}
