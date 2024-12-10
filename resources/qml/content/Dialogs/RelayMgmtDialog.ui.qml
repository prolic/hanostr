import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Controls.Material 2.15
import QtQuick.Layouts 1.15
import QtGraphicalEffects 1.15

import Components 1.0
import Futr 1.0
import HsQML.Model 1.0

Dialog {
    standardButtons: Dialog.Close
    modal: true
    anchors.centerIn: parent

    id: relayMgmtDialog
    width: 800
    height: 700

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: 10
        spacing: 10
        Layout.fillWidth: true

        Text {
            text: qsTr("Relay Management")
            font: Constants.largeFont
            color: Material.primaryTextColor
            Layout.alignment: Qt.AlignHCenter
        }

        ScrollView {
            Layout.fillWidth: true
            Layout.fillHeight: true
            clip: true

            ScrollBar.vertical.policy: ScrollBar.AlwaysOn
            ScrollBar.horizontal.policy: ScrollBar.AlwaysOff

            ColumnLayout {
                width: relayMgmtDialog.width - 60
                spacing: 15

                // DM Relays Section
                Text {
                    text: qsTr("Preferred DM Relays")
                    font: Constants.font
                    color: Material.primaryTextColor
                }

                ListView {
                    id: dmRelaysListView
                    Layout.fillWidth: true
                    Layout.preferredHeight: contentHeight
                    clip: true
                    interactive: false
                    Layout.rightMargin: 25

                    model: AutoListModel {
                        source: ctxRelayMgmt.dmRelays
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.url === newItem.url
                                && oldItem.connectionState === newItem.connectionState
                                && oldItem.connectionRetries === newItem.connectionRetries
                                && oldItem.notices === newItem.notices
                        }
                    }

                    delegate: RowLayout {
                        Layout.fillWidth: true
                        spacing: 10
                        width: dmRelaysListView.width

                        RelayStatusIcon {
                            connectionState: modelData.connectionState
                            connectionRetries: modelData.connectionRetries
                        }

                        Text {
                            text: modelData.url
                            font: Constants.font
                            color: Material.primaryTextColor
                            Layout.fillWidth: true
                        }

                        Button {
                            text: modelData.connectionState === "Disconnected" ? qsTr("Connect") : qsTr("Disconnect")
                            Layout.preferredWidth: 100
                            onClicked: {
                                if (modelData.connectionState === "Disconnected") {
                                    ctxRelayMgmt.connectRelay(modelData.url)
                                } else {
                                    ctxRelayMgmt.disconnectRelay(modelData.url)
                                }
                            }
                        }

                        Button {
                            onClicked: {
                                removeRelayDialog.relayToRemove = modelData.url
                                removeRelayDialog.relayType = "dm"
                                removeRelayDialog.open()
                            }
                            icon.source: "qrc:/icons/close.svg"
                            icon.width: 15
                            icon.height: 15
                            flat: true
                        }
                    }
                }

                RowLayout {
                    id: preferredFooterLayout
                    spacing: 10
                    Layout.fillWidth: true
                    Layout.preferredHeight: addPreferredButton.height
                    Layout.rightMargin: 25

                    Button {
                        id: addPreferredButton
                        Layout.preferredWidth: implicitWidth + 100
                        icon.source: "qrc:/icons/add.svg"
                        text: qsTr("Add DM Relay")
                        onClicked: {
                            newDMRelaysInput.visible = true
                            savePreferredButton.visible = true
                            cancelPreferredButton.visible = true
                            addPreferredButton.visible = false
                            newDMRelaysInput.forceActiveFocus()
                        }
                    }

                    TextField {
                        id: newDMRelaysInput
                        Layout.fillWidth: true
                        placeholderText: qsTr("Enter relay URL (ws:// or wss://)")
                        font: Constants.font
                        visible: false
                        text: "wss://"
                        onVisibleChanged: {
                            if (visible) {
                                text = "wss://"
                            }
                        }
                        onTextChanged: validateUrl(newDMRelaysInput)
                        property bool isValid: false

                        Keys.onReturnPressed: {
                            if (newDMRelaysInput.isValid) {
                                savePreferredButton.clicked()
                            }
                        }
                    }

                    Item {
                        width: 20
                        height: 20
                        visible: newDMRelaysInput.visible && newDMRelaysInput.text.trim() !== "" && newDMRelaysInput.text.trim() !== "wss://"

                        Image {
                            id: dmRelaysIcon
                            anchors.fill: parent
                            source: "qrc:/icons/error.svg"
                            visible: !newDMRelaysInput.isValid
                        }

                        ColorOverlay {
                            anchors.fill: parent
                            source: dmRelaysIcon
                            color: "red"
                            visible: !newDMRelaysInput.isValid
                        }
                    }

                    Button {
                        id: cancelPreferredButton
                        visible: false
                        flat: true
                        icon.source: "qrc:/icons/cancel.svg"
                        onClicked: {
                            newDMRelaysInput.text = ""
                            newDMRelaysInput.visible = false
                            savePreferredButton.visible = false
                            cancelPreferredButton.visible = false
                            addPreferredButton.visible = true
                        }
                    }

                    Button {
                        id: savePreferredButton
                        visible: false
                        icon.source: "qrc:/icons/save.svg"
                        flat: true
                        enabled: newDMRelaysInput.isValid
                        onClicked: {
                            if (newDMRelaysInput.text.trim() !== "") {
                                if (ctxRelayMgmt.addDMRelay(newDMRelaysInput.text.trim())) {
                                    ctxRelayMgmt.connectRelay(newDMRelaysInput.text.trim())
                                }
                                newDMRelaysInput.text = ""
                                newDMRelaysInput.visible = false
                                savePreferredButton.visible = false
                                cancelPreferredButton.visible = false
                                addPreferredButton.visible = true
                            }
                        }
                    }
                }

                Rectangle {
                    Layout.fillWidth: true
                    height: 1
                    color: Material.dividerColor
                    Layout.topMargin: 10
                    Layout.bottomMargin: 10
                }

                // General Relays Section
                Text {
                    text: qsTr("Inbox / Outbox Relays")
                    font: Constants.font
                    color: Material.primaryTextColor
                }

                ListView {
                    id: inboxRelayListView
                    Layout.fillWidth: true
                    Layout.preferredHeight: contentHeight
                    clip: true
                    interactive: false
                    Layout.rightMargin: 25

                    model: AutoListModel {
                        source: ctxRelayMgmt.generalRelays
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.url === newItem.url
                                && oldItem.connectionState === newItem.connectionState
                                && oldItem.isInbox === newItem.isInbox
                                && oldItem.isOutbox === newItem.isOutbox
                                && oldItem.connectionRetries === newItem.connectionRetries
                                && oldItem.notices === newItem.notices
                        }
                    }

                    delegate: RowLayout {
                        Layout.fillWidth: true
                        spacing: 10
                        width: inboxRelayListView.width

                        RelayStatusIcon {
                            connectionState: modelData.connectionState
                            connectionRetries: modelData.connectionRetries
                        }

                        Text {
                            text: modelData.url
                            font: Constants.font
                            color: Material.primaryTextColor
                            Layout.fillWidth: true
                        }

                        CheckBox {
                            checked: modelData.isInbox
                            text: qsTr("Inbox")
                            enabled: false
                        }

                        CheckBox {
                            checked: modelData.isOutbox
                            text: qsTr("Outbox")
                            enabled: false
                        }

                        Button {
                            text: modelData.connectionState === "Disconnected" ? qsTr("Connect") : qsTr("Disconnect")
                            Layout.preferredWidth: 100
                            onClicked: {
                                if (modelData.connectionState === "Disconnected") {
                                    ctxRelayMgmt.connectRelay(modelData.url)
                                } else {
                                    ctxRelayMgmt.disconnectRelay(modelData.url)
                                }
                            }
                        }

                        Button {
                            onClicked: {
                                removeRelayDialog.relayToRemove = modelData.url
                                removeRelayDialog.relayType = "general"
                                removeRelayDialog.open()
                            }
                            icon.source: "qrc:/icons/close.svg"
                            icon.width: 15
                            icon.height: 15
                            flat: true
                        }
                    }
                }

                RowLayout {
                    spacing: 10
                    Layout.fillWidth: true
                    Layout.rightMargin: 25

                    Button {
                        id: addInboxButton
                        Layout.preferredWidth: implicitWidth + 100
                        icon.source: "qrc:/icons/add.svg"
                        text: qsTr("Add Relay")
                        visible: !newRelayInput.visible
                        onClicked: {
                            newRelayInput.visible = true
                            newRelayInput.forceActiveFocus()
                        }
                    }

                    TextField {
                        id: newRelayInput
                        Layout.fillWidth: true
                        placeholderText: qsTr("Enter relay URL (ws:// or wss://)")
                        font: Constants.font
                        visible: false
                        text: "wss://"
                        onVisibleChanged: {
                            if (visible) {
                                text = "wss://"
                            }
                        }
                        onTextChanged: validateUrl(newRelayInput)
                        property bool isValid: false

                        Keys.onReturnPressed: {
                            if (newRelayInput.isValid) {
                                saveInboxButton.clicked()
                            }
                        }
                    }

                    CheckBox {
                        id: newInboxRelayCheckboxCheckbox
                        checked: true
                        visible: newRelayInput.visible
                        text: qsTr("Inbox")
                    }

                    CheckBox {
                        id: newOutboxRelayCheckbox
                        checked: true
                        visible: newRelayInput.visible
                        text: qsTr("Outbox")
                    }

                    Item {
                        width: 20
                        height: 20
                        visible: newRelayInput.visible && newRelayInput.text.trim() !== "" && newRelayInput.text.trim() !== "wss://"

                        Image {
                            id: inboxRelayIcon
                            anchors.fill: parent
                            source: "qrc:/icons/error.svg"
                            visible: !newRelayInput.isValid
                        }

                        ColorOverlay {
                            anchors.fill: parent
                            source: inboxRelayIcon
                            color: "red"
                            visible: !newRelayInput.isValid
                        }
                    }

                    Button {
                        id: cancelInboxButton
                        visible: newRelayInput.visible
                        flat: true
                        icon.source: "qrc:/icons/cancel.svg"
                        onClicked: {
                            newRelayInput.text = ""
                            newRelayInput.visible = false
                        }
                    }

                    Button {
                        id: saveInboxButton
                        visible: newRelayInput.visible
                        icon.source: "qrc:/icons/save.svg"
                        flat: true
                        enabled: newRelayInput.isValid
                        onClicked: {
                            if (newRelayInput.text.trim() !== "") {
                                if (ctxRelayMgmt.addGeneralRelay(
                                    newRelayInput.text.trim(),
                                    newInboxRelayCheckboxCheckbox.checked,
                                    newOutboxRelayCheckbox.checked
                                )) {
                                    ctxRelayMgmt.connectRelay(newRelayInput.text.trim())
                                }
                                newRelayInput.text = ""
                                newRelayInput.visible = false
                            }
                        }
                    }
                }

                Rectangle {
                    Layout.fillWidth: true
                    height: 1
                    color: Material.dividerColor
                    Layout.topMargin: 10
                    Layout.bottomMargin: 10
                }

                // Temp Relays Section
                Text {
                    text: qsTr("Temporary Connected Relays")
                    font: Constants.font
                    color: Material.primaryTextColor
                }

                ListView {
                    id: tempRelayListView
                    Layout.fillWidth: true
                    Layout.preferredHeight: contentHeight
                    clip: true
                    interactive: false
                    Layout.rightMargin: 25

                    model: AutoListModel {
                        source: ctxRelayMgmt.tempRelays
                        mode: AutoListModel.ByKey
                        equalityTest: function (oldItem, newItem) {
                            return oldItem.url === newItem.url
                                && oldItem.connectionState === newItem.connectionState
                                && oldItem.connectionRetries === newItem.connectionRetries
                                && oldItem.notices === newItem.notices
                        }
                    }

                    delegate: RowLayout {
                        Layout.fillWidth: true
                        spacing: 10
                        width: tempRelayListView.width

                        RelayStatusIcon {
                            connectionState: modelData.connectionState
                            connectionRetries: modelData.connectionRetries
                        }

                        Text {
                            text: modelData.url
                            font: Constants.font
                            color: Material.primaryTextColor
                            Layout.fillWidth: true
                        }

                        Button {
                            text: qsTr("Disconnect")
                            Layout.preferredWidth: 100
                            onClicked: ctxRelayMgmt.disconnectRelay(modelData.url)
                        }
                    }
                }
            }
        }
    }

    RemoveRelayDialog {
        id: removeRelayDialog
    }

    function validateUrl(inputField) {
        var urlRegex = /^(ws:\/\/|wss:\/\/)[\w-]+(\.[\w-]+)+([\w.,@?^=%&:/~+#-]*[\w@?^=%&/~+#-])?$/
        inputField.isValid = urlRegex.test(inputField.text.trim())
    }
}
