function togglePasswordVisibility(passwordId, iconId) {
    var passwordInput = document.getElementById(passwordId);
    var icon = document.getElementById(iconId);
    if (passwordInput.type === "password") {
        passwordInput.type = "text";
        icon.classList.add("bi-eye-slash");
        icon.classList.remove("bi-eye");
    } else {
        passwordInput.type = "password";
        icon.classList.add("bi-eye");
        icon.classList.remove("bi-eye-slash");
    }
}

function validateUpdateForm() {
    var firstName = document.getElementById("first_name").value;
    var lastName = document.getElementById("last_name").value;
    var company = document.getElementById("company").value;
    var email = document.getElementById("email").value;
    var password = document.getElementById("password").value;
    var confirmPassword = document.getElementById("confirm_password").value;

    var valid = true;

    if (firstName === "") {
        document.getElementById("firstNameError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("firstNameError").style.display = "none";
    }

    if (lastName === "") {
        document.getElementById("lastNameError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("lastNameError").style.display = "none";
    }

    if (company === "") {
        document.getElementById("companyError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("companyError").style.display = "none";
    }

    if (email === "") {
        document.getElementById("emailError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("emailError").style.display = "none";
    }

    if (password === "") {
        document.getElementById("passwordError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("passwordError").style.display = "none";
    }

    if (confirmPassword === "") {
        document.getElementById("confirmPasswordError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("confirmPasswordError").style.display = "none";
    }

    if (password !== confirmPassword) {
        document.getElementById("passwordMatchError").style.display = "block";
        valid = false;
    } else {
        document.getElementById("passwordMatchError").style.display = "none";
    }

    if (valid) {
        // Enviar el formulario
        alert("Formulario válido. Procesando actualización...");
    }
}
